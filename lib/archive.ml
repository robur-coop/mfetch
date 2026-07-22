let src = Logs.Src.create "mfetch.archive"
let ( let@ ) finally fn = Fun.protect ~finally fn
let ( let* ) = Result.bind
let inhibit fn = try fn () with _exn -> ()
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Digest = struct
  type t = { feed : string -> unit; get : unit -> bool }

  let of_hash : type a. a Digestif.hash -> a Digestif.t -> t =
   fun k expected ->
    let module Hash = (val Digestif.module_of k) in
    let ctx = ref (Hash.init ()) in
    let feed str = ctx := Hash.feed_string !ctx str in
    let get () =
      let result = Digestif.of_digest (module Hash) (Hash.get !ctx) in
      Digestif.equal k expected result in
    { feed; get }
end

module Log = (val Logs.src_log src : Logs.LOG)

let tbz_into dst from =
  let cmd = Fmt.str "tar -xjf - -C %s" (Filename.quote (Fpath.to_string dst)) in
  let oc = Unix.open_process_out cmd in
  let fn () = Flux.Source.each (output_string oc) from in
  let finally () = ignore (Unix.close_process_out oc) in
  Fun.protect ~finally fn ;
  match Unix.close_process_out oc with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Fmt.failwith "tar -xj exited with %d" n
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> Fmt.failwith "tar -xj killed"

let path ~dst name =
  let segs = String.split_on_char '/' name in
  if (name <> "" && name.[0] = '/') || List.mem ".." segs
  then Fmt.failwith "Unsafe path %S in the given archive" name ;
  let segs = List.filter (fun seg -> seg <> "" && seg <> ".") segs in
  List.fold_left Fpath.add_seg dst segs

let mkdir_p dirpath =
  Bos.OS.Dir.create ~path:true dirpath
  |> Result.map_error (fun (`Msg msg) -> msg)
  |> Result.error_to_failure
  |> ignore

let unzip_into dst from =
  let filename = Filename.temp_file "mfetch" ".zip" in
  let@ () = fun () -> inhibit @@ fun () -> Sys.remove filename in
  let via = Flux.Flow.identity and into = Flux.Sink.file ~filename in
  let (), leftover = Flux.Stream.run ~from ~via ~into in
  Option.iter Flux.Source.dispose leftover ;
  match Flux_unzip.of_filename filename with
  | Error (`Msg msg) -> failwith msg
  | Ok t ->
      let fn (entry : Flux_unzip.entry) =
        let name = entry.Flux_unzip.filepath in
        let filepath = path ~dst name in
        if String.length name > 0 && name.[String.length name - 1] = '/'
        then mkdir_p filepath
        else
          let () = mkdir_p (Fpath.parent filepath) in
          let stream = Flux_unzip.stream t entry in
          let filename = Fpath.to_string filepath in
          Flux.Stream.into (Flux.Sink.file ~filename) stream in
      List.iter fn (Flux_unzip.entries t)

let write_tar_entry dst ((hdr : Tar.Header.t), contents) =
  let filepath = path ~dst hdr.Tar.Header.file_name in
  match hdr.Tar.Header.link_indicator with
  | Tar.Header.Link.Directory -> mkdir_p filepath
  | Tar.Header.Link.Normal ->
      let mode = hdr.Tar.Header.file_mode in
      let mode = if mode land 0o100 <> 0 then 0o755 else 0o644 in
      mkdir_p (Fpath.parent filepath) ;
      Bos.OS.File.write ~mode filepath contents
      |> Result.map_error (fun (`Msg msg) -> msg)
      |> Result.error_to_failure
  | Tar.Header.Link.Symbolic ->
      mkdir_p (Fpath.parent filepath) ;
      let target = hdr.Tar.Header.link_name in
      if target <> "" && target.[0] = '/'
      then
        Log.warn (fun m ->
            m "Skip the symbolic link %s -> %s (absolute target)"
              hdr.Tar.Header.file_name target)
      else
        let () = inhibit @@ fun () -> Unix.unlink (Fpath.to_string filepath) in
        Unix.symlink target (Fpath.to_string filepath)
  | Tar.Header.Link.Hard -> begin
      mkdir_p (Fpath.parent filepath) ;
      let target = path ~dst hdr.Tar.Header.link_name in
      try Unix.link (Fpath.to_string target) (Fpath.to_string filepath)
      with exn ->
        Log.warn (fun m ->
            m "Skip the hard link %s -> %s (%s)" hdr.Tar.Header.file_name
              hdr.Tar.Header.link_name (Printexc.to_string exn))
    end
  | _ ->
      Log.warn (fun m ->
          m "Skip the unsupported entry %s" hdr.Tar.Header.file_name)

let untar_into ~kind dst from =
  let via =
    match kind with
    | Edn.Tar_gz ->
        let open Flux.Flow in
        bstr ~len:0x1000 << Flux_gz.inflate << Flux_tar.untar
    | Tar -> Flux_tar.untar
    | _ -> assert false in
  let into = Flux.Sink.fold (fun () entry -> write_tar_entry dst entry) () in
  let (), leftover = Flux.Stream.run ~from ~via ~into in
  Option.iter Flux.Source.dispose leftover

let is_redirection (resp : Httpcats.response) =
  H2.Status.is_redirection resp.Httpcats.status

let once fn =
  let called = ref false in
  fun v ->
    if not !called
    then begin
      called := true ;
      fn v
    end

let unarchive kind dst from =
  match kind with
  | Edn.Tar_gz | Tar -> untar_into ~kind dst from
  | Zip -> unzip_into dst from
  | Tar_bz2 -> tbz_into dst from

let promote ~tmp ~into =
  let entries = Sys.readdir (Fpath.to_string tmp) in
  match entries with
  | [| single |] when Sys.is_directory Fpath.(to_string (tmp / single)) ->
      Unix.rename Fpath.(to_string (tmp / single)) (Fpath.to_string into) ;
      Unix.rmdir (Fpath.to_string tmp)
  | _ -> Unix.rename (Fpath.to_string tmp) (Fpath.to_string into)

let download ~resolver ?(checksum = []) ?(reporter = ignore)
    ?(on_total = ignore) (uri, kind, into) =
  let* () =
    if Sys.file_exists (Fpath.to_string into)
    then error_msgf "%a already exists" Fpath.pp into
    else Ok () in
  let dirpath = Fpath.parent into in
  let* _ = Bos.OS.Dir.create ~path:true dirpath in
  let tmp = Fpath.(dirpath / Fmt.str ".mfetch.%s.tmp" (basename into)) in
  let* _ = Bos.OS.Dir.delete ~recurse:true tmp in
  let* _ = Bos.OS.Dir.create ~path:true tmp in
  let q = Flux.Bqueue.(create with_close_and_halt) 0x7ff in
  let push str = inhibit @@ fun () -> Flux.Bqueue.put q str in
  let on_total = once on_total in
  let feeders =
    List.map (fun (Opam.Hash (k, e)) -> Digest.of_hash k e) checksum in
  let fn _meta _req (resp : Httpcats.response) () = function
    | Some str when not (is_redirection resp) ->
        let hdrs = resp.Httpcats.headers in
        let content_length = Httpcats.Headers.get hdrs "content-length" in
        let total = Option.bind content_length int_of_string_opt in
        on_total total ;
        List.iter (fun { Digest.feed; _ } -> feed str) feeders ;
        reporter (String.length str) ;
        push str
    | _ -> () in
  let prm =
    Miou.async @@ fun () ->
    let@ () = fun () -> inhibit @@ fun () -> Flux.Bqueue.close q in
    Httpcats.request ~resolver ~follow_redirect:true ~fn ~uri () in
  let from = Flux.Source.bqueue ~stop:`Halt q in
  let result =
    try
      unarchive kind tmp from ;
      Ok ()
    with
    | Failure msg -> Error (`Msg msg)
    | exn -> error_msgf "%s" (Printexc.to_string exn) in
  Flux.Source.dispose from ;
  let result =
    let* () = result in
    let* () =
      match Miou.await prm with
      | Ok (Ok _resp) -> Ok ()
      | Ok (Error err) -> error_msgf "%s: %a" uri Httpcats.pp_error err
      | Error exn -> error_msgf "%s: %s" uri (Printexc.to_string exn) in
    let fn acc { Digest.get; _ } =
      match acc with
      | Ok () when get () -> Ok ()
      | _ -> error_msgf "Invalid checksum for %s" uri in
    let* () = List.fold_left fn (Ok ()) feeders in
    match promote ~tmp ~into with
    | () -> Ok ()
    | exception exn ->
        error_msgf "Unable to promote %a: %s" Fpath.pp into
          (Printexc.to_string exn) in
  match result with
  | Ok _ as value -> value
  | Error err ->
      let _ = Bos.OS.Dir.delete ~recurse:true tmp in
      Error err

let excluded = [ ".git"; ".hg"; "_darcs"; "_build"; "_opam"; ".mfetch.state" ]

let rec copy_tree ~reporter ~src ~dst =
  mkdir_p dst ;
  let entries = Sys.readdir (Fpath.to_string src) in
  Array.sort String.compare entries ;
  let fn entry =
    if not (List.mem entry excluded)
    then begin
      let src = Fpath.(src / entry) and dst = Fpath.(dst / entry) in
      let stat = Unix.lstat (Fpath.to_string src) in
      match stat.Unix.st_kind with
      | Unix.S_DIR -> copy_tree ~reporter ~src ~dst
      | Unix.S_LNK ->
          Unix.symlink
            (Unix.readlink (Fpath.to_string src))
            (Fpath.to_string dst)
      | Unix.S_REG ->
          let mode =
            if stat.Unix.st_perm land 0o100 <> 0 then 0o755 else 0o644 in
          let ic = In_channel.open_bin (Fpath.to_string src) in
          let@ () = fun () -> In_channel.close ic in
          let oc =
            open_out_gen
              [ Open_wronly; Open_creat; Open_trunc; Open_binary ]
              mode (Fpath.to_string dst) in
          let@ () = fun () -> close_out oc in
          let buf = Bytes.create 0x7ff in
          let rec go () =
            let len = In_channel.input ic buf 0 (Bytes.length buf) in
            if len > 0
            then begin
              output_string oc (Bytes.sub_string buf 0 len) ;
              reporter len ;
              go ()
            end in
          go ()
      | _ -> Log.warn (fun m -> m "Skip the special file %a" Fpath.pp src)
    end in
  Array.iter fn entries

let copy ?(reporter = ignore) ~src into =
  let* () =
    if Sys.file_exists (Fpath.to_string into)
    then error_msgf "%a already exists" Fpath.pp into
    else if
      not
        (Sys.file_exists (Fpath.to_string src)
        && Sys.is_directory (Fpath.to_string src))
    then error_msgf "%a is not a directory" Fpath.pp src
    else Ok () in
  let dirpath = Fpath.parent into in
  let* _ = Bos.OS.Dir.create ~path:true dirpath in
  let tmp = Fpath.(dirpath / Fmt.str ".mfetch.%s.tmp" (basename into)) in
  let* _ = Bos.OS.Dir.delete ~recurse:true tmp in
  match copy_tree ~reporter ~src ~dst:tmp with
  | () ->
      begin try
        Unix.rename (Fpath.to_string tmp) (Fpath.to_string into) ;
        Ok ()
      with exn ->
        let _ = Bos.OS.Dir.delete ~recurse:true tmp in
        error_msgf "Unable to promote %a: %s" Fpath.pp into
          (Printexc.to_string exn)
      end
  | exception exn ->
      let _ = Bos.OS.Dir.delete ~recurse:true tmp in
      error_msgf "Unable to copy %a: %s" Fpath.pp src (Printexc.to_string exn)

let src = Logs.Src.create "mfetch.git"
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind
let inhibit fn = try fn () with _exn -> ()
let ( let@ ) finally fn = Fun.protect ~finally fn

type kind = [ `Branch of string | `Tag of string | `Detached ]
type error = [ Smart.error | `Msg of string ]

let chop ~prefix str =
  if String.starts_with ~prefix str
  then
    let off = String.length prefix in
    let len = String.length str - String.length prefix in
    String.sub str off len
  else str

let is_hex str =
  let fn = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false in
  String.length str = 40 && String.for_all fn str

module Log = (val Logs.src_log src : Logs.LOG)

let pp_error ppf = function
  | #Smart.error as err -> Smart.pp_error ppf err
  | `Msg msg -> Fmt.string ppf msg

let want refs = function
  | None ->
      let branch =
        match refs.Smart.head_symref with
        | Some symref when String.starts_with ~prefix:"refs/heads/" symref ->
            String.sub symref 11 (String.length symref - 11)
        | _ -> "main" in
      Ok (refs.Smart.head, `Branch branch)
  | Some oid when is_hex oid ->
      let oid = Ohex.decode oid in
      let oid = Carton.Uid.unsafe_of_string oid in
      Ok (oid, `Detached)
  | Some branch -> begin
      let candidates =
        if String.starts_with ~prefix:"refs/heads/" branch
        then [ (branch, `Branch (chop ~prefix:"refs/heads/" branch)) ]
        else if String.starts_with ~prefix:"refs/tags/" branch
        then [ (branch, `Tag (chop ~prefix:"refs/tags/" branch)) ]
        else
          [
            ("refs/heads/" ^ branch, `Branch branch);
            ("refs/tags/" ^ branch, `Tag branch);
          ] in
      let fn (name, k) =
        Option.map (fun oid -> (oid, k)) (List.assoc_opt name refs.Smart.refs)
      in
      match List.find_map fn candidates with
      | Some (oid, refname) when is_hex oid ->
          let oid = Ohex.decode oid in
          let oid = Carton.Uid.unsafe_of_string oid in
          Ok (oid, refname)
      | _ -> error_msgf "Branch or tag %S not found" branch
    end

(* through ssh or git-upload-pack *)
let rec through (ic, oc) = function
  | Protocol.Error err -> Result.Error err
  | Return v -> Ok v
  | Read { k; buffer; off; len } ->
      let len = Stdlib.input ic buffer off len in
      let res = if len = 0 then `End else `Len len in
      through (ic, oc) (k res)
  | Write { k; buffer; off; len } ->
      Stdlib.output_substring oc buffer off len ;
      Stdlib.flush oc ;
      through (ic, oc) (k len)

(* collect for http *)
let rec collect buf = function
  | Protocol.Write { k; buffer; off; len } ->
      Buffer.add_substring buf buffer off len ;
      collect buf (k len)
  | state -> state

let rec unroll (q, rem) = function
  | Protocol.Error err -> Result.Error err
  | Return v -> Ok v
  | Write _ -> assert false
  | Read { k; buffer; off; len } -> begin
      let rec next = function
        | "" -> Option.bind (Flux.Bqueue.get q) next
        | str -> Some str in
      match next rem with
      | None -> unroll (q, "") (k `End)
      | Some str ->
          let len = Int.min len (String.length str) in
          Bytes.blit_string str 0 buffer off len ;
          let rem = String.sub str len (String.length str - len) in
          unroll (q, rem) (k (`Len len))
    end

let is_redirection (resp : Httpcats.response) =
  H2.Status.is_redirection resp.Httpcats.status

let request ~resolver ?(meth = `GET) ?(headers = []) ?body uri state =
  let q = Flux.Bqueue.(create with_close_and_halt) 0x7ff in
  let push str = inhibit @@ fun () -> Flux.Bqueue.put q str in
  let fn _meta _req resp () = function
    | Some str when is_redirection resp -> push str
    | _ -> () in
  let prm =
    Miou.async @@ fun () ->
    let@ () = fun () -> inhibit @@ fun () -> Flux.Bqueue.close q in
    let body = Option.map (fun str -> Httpcats.String str) body in
    Httpcats.request ~resolver ~follow_redirect:true ~meth ~headers ?body ~fn
      ~uri () in
  let result = unroll (q, "") state in
  Flux.Bqueue.halt q ;
  let status = Miou.await prm in
  match (result, status) with
  | (Error _ as err), _ -> err
  | Ok v, Ok (Ok (resp, ())) when resp.Httpcats.status = `OK -> Ok v
  | Ok _, Ok (Ok (resp, ())) ->
      error_msgf "Unexpected response %a" H2.Status.pp_hum resp.Httpcats.status
  | Ok _, Ok (Error err) -> error_msgf "%s: %a" uri Httpcats.pp_error err
  | Ok _, Error exn -> error_msgf "%s: %s" uri (Printexc.to_string exn)

let fetch_through_http ~resolver uri ?branch q =
  let headers = [ ("Git-Protocol", "version=2") ] in
  let* _capabilities =
    let ctx = Protocol.ctx () in
    request ~resolver ~headers
      (uri ^ "/info/refs?service=git-upload-pack")
      (Smart.advertisement ctx) in
  let headers =
    [
      ("Content-Type", "application/x-git-upload-pack-request");
      ("Accept", "application/x-git-upload-pack-result");
      ("Git-Protocol", "version=2");
    ] in
  let uri = uri ^ "/git-upload-pack" in
  let post state =
    let buf = Buffer.create 0x7ff in
    let state = collect buf state in
    let body = Buffer.contents buf in
    request ~resolver ~meth:`POST ~headers ~body uri state in
  let* refs = post (Smart.ls_refs (Protocol.ctx ())) in
  let* want, (kind : kind) = want refs branch in
  let* errored = post (Smart.fetch ~want q (Protocol.ctx ())) in
  if errored
  then error_msgf "%s: remote error during fetch" uri
  else Ok (want, kind)

let fetch_with_process ~cmd ?branch q =
  let ctx = Protocol.ctx () in
  let smart =
    let ( let* ) = Protocol.bind in
    let* _capabilities = Smart.advertisement ctx in
    let* refs = Smart.ls_refs ctx in
    match want refs branch with
    | Error (`Msg msg) -> Protocol.error (`Msg msg)
    | Ok (want, kind) ->
        let* errored = Smart.fetch ~want q ctx in
        Protocol.return ((want, kind), errored) in
  let ic, oc = Unix.open_process cmd in
  let result = through (ic, oc) smart in
  let status = Unix.close_process (ic, oc) in
  match (result, status) with
  | Ok ((refname, oid), false), Unix.WEXITED 0 -> Ok (refname, oid)
  | Ok (_, true), _ -> error_msgf "%s: remote error during fetch" cmd
  | Ok _, Unix.WEXITED n -> error_msgf "%s existed with %d" cmd n
  | Ok _, Unix.(WSIGNALED _ | WSTOPPED _) -> error_msgf "%s killed" cmd
  | Error err, _ -> Error err

let fetch_through_ssh ~user ~host ?(port = 22) ~path ?branch q =
  let remote = Fmt.str "%s@%s" user host in
  let cmd = Fmt.str "git-upload-pack '%s'" path in
  let cmd =
    Fmt.str "GIT_PROTOCOL=version=2 ssh -o SendEnv=GIT_PROTOCOL -p %d %s %s"
      port remote (Filename.quote cmd) in
  fetch_with_process ~cmd ?branch q

let fetch_local_git_repository dirpath ?branch q =
  let cmd =
    Fmt.str "GIT_PROTOCOL=version=2 git-upload-pack %s"
      (Filename.quote (Fpath.to_string dirpath)) in
  fetch_with_process ~cmd ?branch q

let digest =
  let open Digestif in
  let feed_bigstring bstr ctx = SHA1.feed_bigstring ctx bstr
  and feed_bytes buf ~off ~len ctx = SHA1.feed_bytes ctx ~off ~len buf in
  let hash =
    {
      Carton.First_pass.feed_bytes;
      feed_bigstring;
      serialize = Fun.compose SHA1.to_raw_string SHA1.get;
      length = SHA1.digest_size;
    } in
  Carton.First_pass.Digest (hash, SHA1.empty)

let identify =
  let open Digestif in
  let pp_kind ppf = function
    | `A -> Fmt.string ppf "commit"
    | `B -> Fmt.string ppf "tree"
    | `C -> Fmt.string ppf "blob"
    | `D -> Fmt.string ppf "tag" in
  let init kind (len : Carton.Size.t) =
    let hdr = Fmt.str "%a %d\000" pp_kind kind (len :> int) in
    SHA1.feed_string SHA1.empty hdr in
  let feed bstr ctx = SHA1.feed_bigstring ctx bstr in
  let ( $ ) = Fun.compose in
  let serialize = SHA1.(Carton.Uid.unsafe_of_string $ to_raw_string $ get) in
  { Carton.First_pass.init; feed; serialize }

let load pack uid =
  let size = Carton.size_of_uid pack ~uid Carton.Size.zero in
  let blob = Carton.Blob.make ~size in
  Carton.of_uid pack blob ~uid

let rec checkout_tree pack ~worktree ~prefix (uid : Carton.Uid.t) acc =
  let ( / ) = Filename.concat in
  let value = load pack uid in
  let* () =
    match Carton.Value.kind value with
    | `B -> Ok ()
    | _ -> error_msgf "Object %s is not a tree" (Ohex.encode (uid :> string))
  in
  let* entries = Git_object.entries ~uid_length:20 (Carton.Value.string value) in
  let fn acc { Git_object.mode; name; uid } =
    let* acc = acc in
    let path = if prefix = "" then name else prefix / name in
    let filepath = Fpath.(worktree // v path) in
    match mode with
    | 0o40000 ->
        let* _ = Bos.OS.Dir.create ~path:true filepath in
        checkout_tree pack ~worktree ~prefix:path uid acc
    | 0o120000 ->
        let value = load pack uid in
        let target = Carton.Value.string value in
        let* _ = Bos.OS.Dir.create ~path:true (Fpath.parent filepath) in
        let () = inhibit @@ fun () -> Unix.unlink (Fpath.to_string filepath) in
        Unix.symlink target (Fpath.to_string filepath) ;
        Ok ({ Git_index.path = Fpath.v path; mode = 0o120000; uid } :: acc)
    | 0o160000 ->
        Log.warn (fun m -> m "Skip the submodule %s" path) ;
        Ok acc
    | mode ->
        let value = load pack uid in
        let* () =
          match Carton.Value.kind value with
          | `C -> Ok ()
          | _ ->
              error_msgf "Object %s is not a blob" (Ohex.encode (uid :> string))
        in
        let perm = if mode land 0o100 <> 0 then 0o755 else 0o644 in
        let mode = if mode land 0o100 <> 0 then 0o100755 else 0o100644 in
        let* _ = Bos.OS.Dir.create ~path:true (Fpath.parent filepath) in
        let* () =
          Bos.OS.File.write ~mode:perm filepath (Carton.Value.string value)
        in
        Ok ({ Git_index.path = Fpath.v path; mode; uid } :: acc) in
  List.fold_left fn (Ok acc) entries

let checkout ~git_dir ~worktree ~(head : Carton.Uid.t) (pack, idx) =
  let idx = Carton_miou_unix.index ~hash_length:20 ~ref_length:20 idx in
  let index (uid : Carton.Uid.t) =
    let uid = Classeur.unsafe_uid_of_string (uid :> string) in
    Carton.Local (Classeur.find_offset idx uid) in
  let pack = Carton_miou_unix.make ~ref_length:20 ~index pack in
  let@ () =
   fun () ->
    let fd, _ = Carton.fd pack in
    Unix.close fd in
  let rec peel_the_onion hash depth =
    if depth > 5
    then error_msgf "Too many nested tags from %s" (head :> string)
    else
      let value = load pack hash in
      match Carton.Value.kind value with
      | `A -> Ok (hash, value)
      | `D ->
          begin match Git_object.target_of_tag (Carton.Value.string value) with
          | Some hash -> peel_the_onion hash (depth + 1)
          | None ->
              error_msgf "Invalid tag object: %s" (Ohex.encode (hash :> string))
          end
      | _ ->
          error_msgf "%s does not reference a commit"
            (Ohex.encode (hash :> string)) in
  let* hash, commit = peel_the_onion head 0 in
  let* tree =
    match Git_object.tree_of_commit (Carton.Value.string commit) with
    | None ->
        error_msgf "Invalid commit object: %s" (Ohex.encode (hash :> string))
    | Some tree -> Ok tree in
  let* entries = checkout_tree pack ~worktree ~prefix:"" tree [] in
  match Git_index.write ~git_dir ~worktree entries with
  | () -> Ok hash
  | exception exn ->
      error_msgf "Unable to write the git index: %s" (Printexc.to_string exn)

let entries status =
  let fn = function
    | Carton.Resolved_base { cursor; uid; crc; _ } ->
        let offset = Int64.of_int cursor in
        let uid = Classeur.unsafe_uid_of_string (uid :> string) in
        { Classeur.Encoder.crc; offset; uid }
    | Carton.Resolved_node { cursor; uid; crc; _ } ->
        let offset = Int64.of_int cursor in
        let uid = Classeur.unsafe_uid_of_string (uid :> string) in
        { Classeur.Encoder.crc; offset; uid }
    | Carton.Unresolved_base _ | Carton.Unresolved_node _ ->
        failwith "Thin (or malformed) PACK file" in
  let entries = Array.map fn status in
  let compare (a : Classeur.Encoder.entry) (b : Classeur.Encoder.entry) =
    String.compare (a.uid :> string) (b.uid :> string) in
  Array.sort compare entries ;
  entries

let write_index idx hash entries =
  let oc = open_out_bin (Fpath.to_string idx) in
  Fun.protect ~finally:(fun () -> close_out oc) @@ fun () ->
  let encoder =
    Classeur.Encoder.encoder (`Channel oc) ~digest ~pack:hash ~ref_length:20
      entries in
  match Classeur.Encoder.encode encoder `Await with
  | `Ok -> ()
  | `Partial -> assert false

let write_refs ~git_dir ~origin ((uid : Carton.Uid.t), kind) =
  let uid = Ohex.encode (uid :> string) in
  let core =
    "[core]\n\
     \trepositoryformatversion = 0\n\
     \tfilemode = true\n\
     \tbare = false\n\
     \tlogallrefupdates = true\n" in
  let remote =
    Fmt.str
      "[remote \"origin\"]\n\
       \turl = %s\n\
       \tfetch = +refs/heads/*:refs/remotes/origin/*\n"
      origin in
  let writef filepath =
    let _ = Bos.OS.Dir.create ~path:true (Fpath.parent filepath) in
    Bos.OS.File.writef filepath in
  match kind with
  | `Branch branch ->
      let* () = writef Fpath.(git_dir / "HEAD") "ref: refs/heads/%s\n" branch in
      let* () = writef Fpath.(git_dir / "refs" / "heads" / branch) "%s\n" uid in
      let* () =
        writef
          Fpath.(git_dir / "refs" / "remotes" / "origin" / branch)
          "%s\n" uid in
      let section =
        Fmt.str "[branch \"%s\"]\n\tremote = origin\n\tmerge = refs/heads/%s\n"
          branch branch in
      writef Fpath.(git_dir / "config") "%s%s%s" core remote section
  | `Tag tag ->
      let* () = writef Fpath.(git_dir / "refs" / "tags" / tag) "%s\n" uid in
      writef Fpath.(git_dir / "config") "%s%s" core remote
  | `Detached -> writef Fpath.(git_dir / "config") "%s%s" core remote

let checkout ~origin ~into (((head : Carton.Uid.t), (kind : kind)) as reference)
    pack =
  let git_dir = Fpath.(into / ".git") in
  let* _ = Bos.OS.Dir.create ~path:true Fpath.(git_dir / "objects" / "pack") in
  let* _ = Bos.OS.Dir.create ~path:true Fpath.(git_dir / "refs") in
  let cfg = Carton_miou_unix.config ~ref_length:20 (Carton.Identify identify) in
  let* status, hash =
    match Carton_miou_unix.verify_from_pack ~cfg ~digest (Fpath.v pack) with
    | v -> Ok v
    | exception exn ->
        error_msgf "Invalid PACK file: %s" (Printexc.to_string exn) in
  let dst =
    let open Fpath in
    git_dir / "objects" / "pack" / Fmt.str "pack-%s.pack" (Ohex.encode hash)
  in
  let idx =
    let open Fpath in
    git_dir / "objects" / "pack" / Fmt.str "pack-%s.idx" (Ohex.encode hash)
  in
  let* entries =
    match entries status with
    | entries -> Ok entries
    | exception Failure msg -> error_msgf "%s" msg in
  write_index idx hash entries ;
  Unix.rename pack (Fpath.to_string dst) ;
  let* () = write_refs ~git_dir ~origin reference in
  let* head = checkout ~git_dir ~worktree:into ~head (dst, idx) in
  let* () =
    match kind with
    | `Tag _ | `Detached ->
        Bos.OS.File.writef
          Fpath.(git_dir / "HEAD")
          "%s\n"
          (Ohex.encode (head :> string))
    | `Branch _ -> Ok () in
  Ok head

type ssh = { user : string; host : string; port : int option; path : string }

let clone ~resolver ~remote ?branch ?(reporter = ignore) ~origin into =
  let* () =
    if Sys.file_exists (Fpath.to_string into)
    then error_msgf "%a already exists" Fpath.pp into
    else Ok () in
  let tmp =
    Fpath.(parent into / Fmt.str ".mfetch.%s.tmp" (Fpath.basename into)) in
  let* _ = Bos.OS.Dir.delete ~recurse:true tmp in
  let* _ = Bos.OS.Dir.create ~path:true tmp in
  let pack =
    Filename.temp_file ~temp_dir:(Fpath.to_string tmp) ".mfetch" ".pack" in
  let q = Flux.Bqueue.(create with_close) 0x7ff in
  let consumer =
    Miou.async @@ fun () ->
    let from = Flux.Source.bqueue q in
    let via = Flux.Flow.tap (fun str -> reporter (String.length str)) in
    let into = Flux.Sink.file ~filename:pack in
    let (), leftover = Flux.Stream.run ~from ~via ~into in
    Option.iter Flux.Source.dispose leftover in
  let reference =
    match remote with
    | `HTTP uri -> fetch_through_http ~resolver uri ?branch q
    | `SSH { user; host; port; path } ->
        fetch_through_ssh ~user ~host ?port ~path ?branch q
    | `Local dirpath -> fetch_local_git_repository dirpath ?branch q in
  let () = inhibit @@ fun () -> Flux.Bqueue.close q in
  let result = Miou.await consumer in
  let result =
    let* reference : Carton.Uid.t * kind = reference in
    let* () =
      match result with
      | Ok () -> Ok ()
      | Error exn ->
          error_msgf "Unable to store the PACK file: %s"
            (Printexc.to_string exn) in
    let* head = checkout ~origin ~into:tmp reference pack in
    match Unix.rename (Fpath.to_string tmp) (Fpath.to_string into) with
    | () -> Ok head
    | exception exn ->
        error_msgf "Unable to promote %a: %s" Fpath.pp into
          (Printexc.to_string exn) in
  match result with
  | Ok _ as value -> value
  | Error err ->
      let _ = Bos.OS.Dir.delete ~recurse:true tmp in
      let () = inhibit @@ fun () -> Sys.remove pack in
      Error err

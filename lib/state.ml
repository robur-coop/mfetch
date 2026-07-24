let ( let@ ) finally fn = Fun.protect ~finally fn
let ( let* ) = Result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

type entry = {
  target : string;
  edns : Edn.t list;
  source : string option;
  version : string option;
  k : [ `Archive | `Git ];
  commit : Carton.Uid.t option;
  hash : Digestif.SHA256.t;
}

type t = entry list

let hash_of_tree root =
  let ctx = ref (Digestif.SHA256.init ()) in
  let feed str = ctx := Digestif.SHA256.feed_string !ctx str in
  let feed_file filepath =
    let ic = In_channel.open_bin (Fpath.to_string filepath) in
    let@ () = fun () -> In_channel.close ic in
    let buf = Bytes.create 0x7ff in
    let rec go () =
      let len = In_channel.input ic buf 0 (Bytes.length buf) in
      if len > 0
      then begin
        ctx := Digestif.SHA256.feed_bytes !ctx ~off:0 ~len buf ;
        go ()
      end in
    go () in
  let rec walk relpath dirpath =
    let entries = Sys.readdir (Fpath.to_string dirpath) in
    Array.sort String.compare entries ;
    let fn entry =
      if entry <> ".git"
      then begin
        let relpath =
          if relpath = "" then entry else Filename.concat relpath entry in
        let filepath = Fpath.(dirpath / entry) in
        let stat = Unix.lstat (Fpath.to_string filepath) in
        match stat.Unix.st_kind with
        | Unix.S_DIR -> walk relpath (Fpath.to_dir_path filepath)
        | Unix.S_LNK ->
            let value = Unix.readlink (Fpath.to_string filepath) in
            let str =
              Fmt.str "%s\x00l\x00%d\x00%s" relpath (String.length value) value
            in
            feed str
        | Unix.S_REG ->
            let exec = stat.Unix.st_perm land 0o100 <> 0 in
            let str =
              Fmt.str "%s\x00f%s\x00%d\x00" relpath
                (if exec then "x" else "")
                stat.Unix.st_size in
            feed str ;
            feed_file filepath
        | _ -> ()
      end in
    Array.iter fn entries in
  match walk "" root with
  | () -> Ok (Digestif.SHA256.get !ctx)
  | exception Sys_error err -> error_msgf "%s" err
  | exception Unix.Unix_error (err, _, _) ->
      error_msgf "%a: %s" Fpath.pp root (Unix.error_message err)

let kind_of_string = function
  | "archive" -> Ok `Archive
  | "git" -> Ok `Git
  | v -> error_msgf "Invalid type: %s" v

let string_of_kind = function `Archive -> "archive" | `Git -> "git"

let of_bcfg filepath cfg =
  let fields name { Bcfg.children; _ } =
    let fn = function
      | { Bcfg.name = name'; parameters = [ v ]; _ } when name' = name -> Some v
      | _ -> None in
    List.filter_map fn children in
  let field name directive =
    match fields name directive with v :: _ -> Some v | [] -> None in
  let require name directive =
    let none = msgf "%a: missing %s field" Fpath.pp filepath name in
    field name directive |> Option.to_result ~none in
  let fn acc directive =
    let* acc = acc in
    match directive with
    | { Bcfg.name = "version"; parameters = [ "1" ]; _ } -> Ok acc
    | { Bcfg.name = "version"; parameters; _ } ->
        error_msgf "%a: unsupported version %a" Fpath.pp filepath
          Fmt.(Dump.list string)
          parameters
    | { Bcfg.name = "package"; parameters = [ target ]; _ } ->
        let* edns =
          match fields "endpoint" directive with
          | [] ->
              error_msgf "%a: missing \"endpoint\" field for %s" Fpath.pp
                filepath target
          | edns ->
              let fn acc edn =
                let* acc = acc in
                let* edn = Edn.of_string edn in
                Ok (edn :: acc) in
              List.fold_left fn (Ok []) edns in
        let source = field "source" directive in
        let version = field "version" directive in
        let* k = require "type" directive in
        let* k = kind_of_string k in
        let commit = field "commit" directive in
        let commit =
          match Option.map Ohex.decode commit with
          | Some commit -> Some (Carton.Uid.unsafe_of_string commit)
          | None -> None
          | exception _exn -> None in
        (* TODO(dinosaure): verify our [commit]. *)
        let* hash = require "hash" directive in
        let* hash =
          match Digestif.SHA256.of_hex_opt hash with
          | Some hash -> Ok hash
          | None -> error_msgf "%a: invalid hash %S" Fpath.pp filepath hash
        in
        Ok ({ target; edns; source; version; k; commit; hash } :: acc)
    | _ -> Ok acc in
  let* entries = List.fold_left fn (Ok []) cfg in
  Ok (List.rev entries)

let to_bcfg entries =
  let directive ?(children = []) name parameters =
    { Bcfg.name; parameters; children } in
  let fn { target; edns; source; version; k; commit; hash } =
    let children =
      List.concat
        [
          List.map (fun edn -> directive "endpoint" [ Edn.to_string edn ]) edns;
          begin match source with
          | Some src -> [ directive "source" [ src ] ]
          | None -> []
          end;
          begin match version with
          | Some v -> [ directive "version" [ v ] ]
          | None -> []
          end;
          [ directive "type" [ string_of_kind k ] ];
          begin match commit with
          | Some commit ->
              [ directive "commit" [ Ohex.encode (commit :> string) ] ]
          | None -> []
          end;
          [ directive "hash" [ Digestif.SHA256.to_hex hash ] ];
        ] in
    directive ~children "package" [ target ] in
  directive "version" [ "1" ] :: List.map fn entries

let load filepath =
  match open_in_bin (Fpath.to_string filepath) with
  | exception Sys_error _ -> Ok []
  | ic ->
      let@ () = fun () -> close_in ic in
      let lexbuf = Lexing.from_channel ~with_positions:true ic in
      let* cfg =
        match Bcfg.parser lexbuf with
        | Ok cfg -> Ok cfg
        | Error err ->
            error_msgf "%a: %a" Fpath.pp filepath Bcfg.pp_error_for_human err
      in
      of_bcfg filepath cfg

let save filepath entries =
  let cfg = to_bcfg entries in
  match open_out_bin (Fpath.to_string filepath) with
  | exception Sys_error err -> error_msgf "%s" err
  | oc ->
      let@ () = fun () -> close_out oc in
      Seq.iter (output_string oc) (Bcfg.emitter cfg) ;
      Ok ()

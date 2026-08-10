let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let ( let* ) = Result.bind
let src = Logs.Src.create "mfetch.edn"

module Log = (val Logs.src_log src : Logs.LOG)
module Set = Set.Make (String)

type archive = Tar_gz | Tar_bz2 | Tar | Zip

type t =
  | Opam of { name : string; version : string option }
  | Archive of { uri : string; archive : archive; checksum : Opam.hash list }
  | Git_http of { uri : string; branch : string option }
  | Git_ssh of {
      user : string;
      host : string;
      port : int option;
      path : string;
      branch : string option;
    }
  | Git_local of { dirpath : Fpath.t; branch : string option }
  | Local of { dirpath : Fpath.t }

(* ugly! *)
external reraise : exn -> 'a = "%reraise"

let chop_archive_extension filepath =
  let exception Return of string in
  let fn ext =
    try
      let name = Filename.chop_suffix filepath ext in
      raise (Return name)
    with
    | Return name -> reraise (Return name)
    | _ -> () in
  try
    List.iter fn [ ".tar.gz"; ".tbz"; ".zip" ] ;
    filepath
  with Return name -> name

(* NOTE(dinosaure): clean-up queries and anchor (often used for commits) from
   an uri (git+http(s)? or from an archive). *)
let path_of_uri uri =
  let uri =
    match String.index_opt uri '?' with
    | None -> uri
    | Some idx -> String.sub uri 0 idx in
  match String.index_opt uri '#' with
  | None -> uri
  | Some idx -> String.sub uri 0 idx

let name = function
  | Opam { name; _ } -> name
  | Archive { uri; _ } ->
      chop_archive_extension (Filename.basename (path_of_uri uri))
  | Git_http { uri; _ } ->
      let path = path_of_uri uri in
      Filename.remove_extension (Filename.basename path)
  | Git_ssh { path; _ } -> Filename.remove_extension (Filename.basename path)
  | Git_local { dirpath; _ } -> Fpath.(basename (rem_ext dirpath))
  | Local { dirpath } -> Fpath.basename dirpath

(* git+ssh://user@host(:port)?/path(#branch)? *)
let decode_git_ssh_uri str =
  let uri, branch =
    match List.rev (String.split_on_char '#' str) with
    | [] | [ _ ] -> (str, None)
    | branch :: rem ->
        let uri = String.concat "#" (List.rev rem) in
        (uri, Some branch) in
  match String.split_on_char '@' uri with
  | [] -> error_msgf "Invalid git+ssh endpoint: %S" str
  | user :: rem -> begin
      let host_port_and_path = String.concat "@" rem in
      match String.split_on_char '/' host_port_and_path with
      | [] -> assert false
      | host_and_port :: path ->
          let path = String.concat "/" path in
          let* host, port =
            match String.split_on_char ':' host_and_port with
            | [] | [ _ ] -> Ok (host_and_port, None)
            | host :: port' ->
                let port' = String.concat ":" port' in
                let port = int_of_string_opt port' in
                let none = msgf "Invalid port: %S" port' in
                let* port = Option.to_result ~none port in
                Ok (host, Some port) in
          Ok (Git_ssh { user; host; port; path; branch })
    end

let cut ~sep str =
  let sep_len = String.length sep in
  if sep_len = 0 then invalid_arg "Edn.cut: sep must be not empty" ;
  let str_len = String.length str in
  let max_sep_idx = sep_len - 1 in
  let max_str_idx = str_len - sep_len in
  let rec check_sep idx k =
    if k > max_sep_idx
    then
      let r_start = idx + sep_len in
      Some (String.sub str 0 idx, String.sub str r_start (str_len - r_start))
    else if str.[idx + k] = sep.[k]
    then check_sep idx (k + 1)
    else scan (idx + 1)
  and scan idx =
    if idx > max_str_idx
    then None
    else if str.[idx] = sep.[0]
    then check_sep idx 1
    else scan (idx + 1) in
  scan 0

(* user@host:path(#branch)? *)
let decode_ssh str =
  let str, branch =
    match List.rev (String.split_on_char '#' str) with
    | [] -> (str, None)
    | [ str ] -> (str, None)
    | branch :: rem -> (String.concat "#" (List.rev rem), Some branch) in
  match String.split_on_char '@' str with
  | [] -> assert false
  | user :: rem ->
      let rem = String.concat "@" rem in
      let* host, path =
        match String.split_on_char ':' rem with
        | [] | [ _ ] -> error_msgf "No given path into your endpoint: %S" str
        | host :: path ->
            let path = String.concat ":" path in
            Ok (host, path) in
      Ok (Git_ssh { user; host; port = None; path; branch })

let decode_package str =
  match String.split_on_char '.' str with
  | [] | [ _ ] -> Ok (Opam { name = str; version = None })
  | name :: version ->
      let version = String.concat "." version in
      Ok (Opam { name; version = Some version })

let is_ssh str = String.index_opt str '@' |> Option.is_some

let is_valid_package_name =
  let fn = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '+' -> true
    | _ -> false in
  String.for_all fn

let is_package str =
  match String.split_on_char '.' str with
  | [] | [ _ ] -> is_valid_package_name str
  | str :: _ -> is_valid_package_name str

let of_string ?(checksum = []) str =
  match cut ~sep:"://" str with
  | Some ("git+ssh", rem) -> decode_git_ssh_uri rem
  | Some ("git+file", str) ->
      let* dirpath, branch =
        match List.rev (String.split_on_char '#' str) with
        | [] | [ _ ] ->
            let* dirpath = Fpath.of_string str in
            Ok (Fpath.to_dir_path dirpath, None)
        | branch :: rem ->
            let dirpath = String.concat "#" (List.rev rem) in
            let* dirpath = Fpath.of_string dirpath in
            Ok (Fpath.to_dir_path dirpath, Some branch) in
      Ok (Git_local { dirpath; branch })
  | Some ("git+http", str) ->
      let uri, branch =
        match List.rev (String.split_on_char '#' str) with
        | [] | [ _ ] -> ("http://" ^ str, None)
        | branch :: rem ->
            let uri = "http://" ^ String.concat "#" (List.rev rem) in
            (uri, Some branch) in
      Ok (Git_http { uri; branch })
  | Some ("git+https", str) ->
      let uri, branch =
        match List.rev (String.split_on_char '#' str) with
        | [] | [ _ ] -> ("https://" ^ str, None)
        | branch :: rem ->
            let uri = "https://" ^ String.concat "#" (List.rev rem) in
            (uri, Some branch) in
      Ok (Git_http { uri; branch })
  | Some ("file", rem) ->
      let* dirpath = Fpath.of_string rem in
      let dirpath = Fpath.to_dir_path dirpath in
      Ok (Local { dirpath })
  | Some (("http" | "https"), _rem) ->
      let* archive =
        match List.rev (String.split_on_char '.' str) with
        | "gz" :: "tar" :: _ -> Ok Tar_gz
        | "bz2" :: "tar" :: _ | "tbz" :: _ -> Ok Tar_bz2
        | "tar" :: _ -> Ok Tar
        | "zip" :: _ -> Ok Zip
        | _ -> error_msgf "Impossible to recoginize the given archive: %S" str
      in
      Ok (Archive { uri = str; archive; checksum })
  | Some _ -> error_msgf "Unrecognized scheme on: %S" str
  | None when is_ssh str -> decode_ssh str
  | None when is_package str -> decode_package str
  | None -> error_msgf "Invalid endpoint: %S" str

let of_string_exn str =
  match of_string str with
  | Ok v -> v
  | Error (`Msg msg) -> Fmt.failwith "%s" msg

let pp_branch ppf = function
  | None -> ()
  | Some branch -> Fmt.pf ppf "#%s" branch

let to_string = function
  | Archive { uri; _ } -> uri
  | Git_http { uri; branch } -> Fmt.str "git+%s%a" uri pp_branch branch
  | Local { dirpath } -> Fmt.str "file://%a" Fpath.pp dirpath
  | Git_local { dirpath; branch } ->
      Fmt.str "git+file://%a%s%a" Fpath.pp (Fpath.parent dirpath)
        (Fpath.basename dirpath) pp_branch branch
  | Opam { name; version = Some version } -> Fmt.str "%s.%s" name version
  | Opam { name; _ } -> name
  | Git_ssh { user; host; port = None; path; branch } ->
      Fmt.str "%s@%s:%s%a" user host path pp_branch branch
  | Git_ssh { user; host; port = Some port; path; branch } ->
      Fmt.str "git+ssh://%s@%s:%d/%s%a" user host port path pp_branch branch

let pp = Fmt.of_to_string to_string
let equal = ( = )

let checksum_of_children =
  let fn = function
    | { Bcfg.name = hash; parameters = [ value ]; _ } ->
        Opam.to_checksum (Fmt.str "%s=%s" hash value)
    | _ -> None in
  List.filter_map fn

let from_filepath filepath =
  let parser ic () =
    let lexbuf = Lexing.from_channel ~with_positions:true ic in
    match Bcfg.parser lexbuf with
    | Error err ->
        error_msgf "%a: %a" Fpath.pp filepath Bcfg.pp_error_for_human err
    | Ok cfg ->
        let fn acc { Bcfg.name; children; _ } =
          let checksum = checksum_of_children children in
          match of_string ~checksum name with
          | Ok edn -> Set.add (to_string edn) acc
          | Error _ ->
              Log.warn (fun m -> m "Ignore endpoint %S" name) ;
              acc in
        let edns = List.fold_left fn Set.empty cfg in
        let edns = Set.elements edns in
        let edns = List.map of_string_exn edns in
        Ok edns in
  match open_in_bin (Fpath.to_string filepath) with
  | ic -> Fun.protect ~finally:(fun () -> close_in ic) (parser ic)
  | exception Sys_error err -> error_msgf "%a: %s" Fpath.pp filepath err

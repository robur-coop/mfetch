let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let ( let* ) = Result.bind

type archive =
  | Tar_gz
  | Tar_bz2
  | Tar
  | Zip

type t =
  | Opam of { name : string; version : string option }
  | Archive of { uri : string; archive : archive }
  | Git_http of { uri : string; branch : string option }
| Git_ssh of { user : string; host : string; port : int option; path : string; branch : string option }
  | Git_local of { dirpath : Fpath.t; branch : string option }
  | Local of { dirpath : Fpath.t }

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
        let* host, port = match String.split_on_char ':' host_and_port with
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
  if sep_len = 0 then invalid_arg "Edn.cut: sep must be not empty";
  let str_len = String.length str in
  let max_sep_idx = sep_len - 1 in
  let max_str_idx = str_len - sep_len in
  let rec check_sep idx k =
    if k > max_sep_idx then
      let r_start = idx + sep_len in
      Some (String.sub str 0 idx, String.sub str r_start (str_len - r_start))
    else
      if str.[idx + k] = sep.[k]
      then check_sep idx (k+1)
      else scan (idx+1)
  and scan idx =
    if idx > max_str_idx then None
    else if str.[idx] = sep.[0]
    then check_sep idx 1
    else scan (idx + 1) in
  scan 0

let decode_ssh str =
  let str, branch = match List.rev (String.split_on_char '#' str) with
    | [] -> str, None
    | [ str ] -> str, None
    | branch :: rem -> String.concat "#" (List.rev rem), Some branch in
  match String.split_on_char '@' str with
  | [] -> assert false
  | user :: rem ->
    let rem = String.concat "@" rem in
    let* host, path = match String.split_on_char ':' rem with
      | [] | [ _ ] -> error_msgf "No given path into your endpoint: %S" str
      | host :: path ->
        let path = String.concat ":" path in
        Ok (host, path) in
    Ok (Git_ssh { user; host; port= None; path; branch })

let directory_exists dirpath =
  let dirname = Fpath.to_string dirpath in
  if Sys.file_exists dirname && Sys.is_directory dirname
  then Ok () else error_msgf "%a is not an existing directory" Fpath.pp dirpath

let is_ssh str = String.index_opt str '@' |> Option.is_some

let of_string str =
  match cut ~sep:"://" str with
  | Some ("git+ssh", rem) -> decode_git_ssh_uri rem
  | Some ("git+file", str) ->
    let* dirpath, branch = match List.rev (String.split_on_char '#' str) with
      | [] | [ _ ] ->
        let* dirpath = Fpath.of_string str in
        Ok (Fpath.to_dir_path dirpath, None)
      | branch :: rem ->
        let dirpath = String.concat "#" (List.rev rem) in
        let* dirpath = Fpath.of_string dirpath in
        Ok (Fpath.to_dir_path dirpath, Some branch) in
    let* () = directory_exists dirpath in
    Ok (Git_local { dirpath; branch })
  | Some ("git+http", str) ->
    let uri, branch = match List.rev (String.split_on_char '#' str) with
      | [] | [ _ ] -> str, None
      | branch :: rem ->
        let uri = "http://" ^ String.concat "#" (List.rev rem) in
        uri, Some branch in
    Ok (Git_http { uri; branch })
  | Some ("git+https", str) ->
    let uri, branch = match List.rev (String.split_on_char '#' str) with
      | [] | [ _ ] -> str, None
      | branch :: rem ->
        let uri = "https://" ^ String.concat "#" (List.rev rem) in
        uri, Some branch in
    Ok (Git_http { uri; branch })
  | Some ("file", rem) ->
    let* dirpath = Fpath.of_string rem in
    let dirpath = Fpath.to_dir_path dirpath in
    let* () = directory_exists dirpath in
    Ok (Local { dirpath })
  | Some (("http" | "https"), _rem) ->
    let* archive = match List.rev (String.split_on_char '.' str) with
      | "gz" :: "tar" :: _ -> Ok Tar_gz
      | "bz2" :: "tar" :: _ -> Ok Tar_bz2
      | "tar" :: _ -> Ok Tar
      | "zip" :: _ -> Ok Zip
      | _ -> error_msgf "Impossible to recoginize the given archive: %S" str in
    Ok (Archive { uri= str; archive })
  | Some _ -> error_msgf "Unrecognized scheme on: %S" str
  | None when is_ssh str -> decode_ssh str
  | None -> error_msgf "Invalid endpoint: %S" str

let pp_branch ppf = function
  | None -> ()
  | Some branch -> Fmt.pf ppf "#%s" branch

let to_string = function
  | Archive { uri; _ } -> uri
  | Git_http { uri; branch } -> Fmt.str "git+%s%a" uri pp_branch branch
  | Local { dirpath } -> Fmt.str "file://%a" Fpath.pp dirpath
  | Git_local { dirpath; branch  } ->
    Fmt.str "git+file://%a%s%a" Fpath.pp (Fpath.parent dirpath) (Fpath.basename dirpath) pp_branch branch
  | Opam { name; version= Some version } -> Fmt.str "%s.%s" name version
  | Opam { name; _ } -> name
  | Git_ssh { user; host; port= None; path; branch } ->
    Fmt.str "%s@%s:%s%a" user host path pp_branch branch
  | Git_ssh { user; host; port= Some port; path; branch } ->
    Fmt.str "git+ssh://%s@%s:%d/%s%a" user host port path pp_branch branch

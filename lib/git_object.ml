(* NOTE(dinosaure): ocaml-git for a poor man... *)

let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let decode_opt str = try Some (Ohex.decode str) with _exn -> None

(* tree <hex>\n *)
let tree_of_commit str =
  match String.index_opt str '\n' with
  | None -> None
  | Some eol ->
      begin match String.split_on_char ' ' (String.sub str 0 eol) with
      | [ "tree"; hex ] -> decode_opt hex
      | _ -> None
      end

(* object <hex>\n *)
let target_of_tag str =
  match String.index_opt str '\n' with
  | None -> None
  | Some eol ->
      begin match String.split_on_char ' ' (String.sub str 0 eol) with
      | [ "object"; hex ] -> decode_opt hex
      | _ -> None
      end

type entry = { mode : int; name : string; uid : string }

let entries ~uid_length str =
  let rec go acc pos =
    if pos >= String.length str
    then Ok (List.rev acc)
    else
      match String.index_from_opt str pos '\000' with
      | None -> error_msgf "Malformed tree object"
      | Some nul ->
          if nul + uid_length >= String.length str
          then error_msgf "Malformed tree object"
          else
            begin match String.index_from_opt str pos ' ' with
            | Some sp when sp < nul -> begin
                let mode = String.sub str pos (sp - pos) in
                let name = String.sub str (sp + 1) (nul - sp - 1) in
                let uid = String.sub str (nul + 1) uid_length in
                match inf_of_string_opt ("0o" ^ mode) with
                | Some mode ->
                    go ({ mode; name; uid } :: acc) (nul + 1 + uid_length)
                | None -> error_msgf "Invalid mode %S" mode
              end
            | None -> error_msgf "Malformed tree object"
            end in
  go [] 0

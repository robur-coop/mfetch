let src = Logs.Src.create "mfetch.lock"
let ( let* ) = Result.bind
let ( let@ ) finally fn = Fun.protect ~finally fn
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

module Log = (val Logs.src_log src : Logs.LOG)

type entry = {
  target : string;  (** Sub-directory of the target directory. *)
  url : string;
      (** An opam URL: the URL of an archive, or [git+<uri>#<commit>]. It is
          self-describing: no opam root is needed to fetch it again. *)
  checksum : Opam.hash list;  (** Empty for Git sources. *)
  version : string option;  (** The opam version, when it comes from one. *)
  edns : Edn.t list;
      (** The specifications of [_mfetch] which resolved to this entry. Kept for
          provenance only. *)
}

type t = { target : string; entries : entry list }

type ls_remote =
  ?branch:string ->
  [ `HTTP of string | `SSH of Git.ssh | `Local of Fpath.t ] ->
  (Carton.Uid.t, [ `Msg of string ]) result

let field_target = "x-mfetch-target"
let field_dirs = "x-mfetch-vendored-dirs"
let field_endpoints = "x-mfetch-endpoints"

open OpamParserTypes.FullPos

let nopos = { filename = ""; start = (0, 0); stop = (0, 0) }
let mk pelem = { pelem; pos = nopos }
let str v = mk (String v)
let lst vs = mk (List (mk vs))
let var name value = mk (Variable (mk name, value))

let to_opamfile ~file_name { target; entries } =
  let dirs =
    let fn { url; target; checksum; _ } =
      let checksum = List.map (Fun.compose str Opam.string_of_hash) checksum in
      lst [ str url; str target; lst checksum ] in
    List.map fn entries in
  let endpoints =
    let fn { target; version; edns; _ } =
      lst
        [
          str target;
          str (Option.value ~default:"" version);
          lst (List.map (Fun.compose str Edn.to_string) edns);
        ] in
    List.map fn entries in
  let file_contents =
    [
      var "opam-version" (str "2.0");
      var field_target (str target);
      var field_dirs (lst dirs);
      var field_endpoints (lst endpoints);
    ] in
  { file_contents; file_name }

let save filepath t =
  let file_name = Fpath.to_string filepath in
  let contents = OpamPrinter.FullPos.opamfile (to_opamfile ~file_name t) in
  match open_out_bin file_name with
  | exception Sys_error err -> error_msgf "%s" err
  | oc ->
      let@ () = fun () -> close_out oc in
      output_string oc contents ;
      Ok ()

let string_of_value ~ctx = function
  | { pelem = String v; _ } -> Ok v
  | _ -> error_msgf "Expected a string for %s" ctx

let strings_of_value ~ctx = function
  | { pelem = List { pelem = vs; _ }; _ } ->
      let fn acc v =
        let* acc = acc in
        let* v = string_of_value ~ctx v in
        Ok (v :: acc) in
      let* vs = List.fold_left fn (Ok []) vs in
      Ok (List.rev vs)
  | _ -> error_msgf "Expected a list of strings for %s" ctx

let variable name { file_contents; _ } =
  let fn { pelem; _ } =
    match pelem with
    | Variable ({ pelem = name'; _ }, value) when name' = name -> Some value
    | _ -> None in
  List.find_map fn file_contents

let entries_of_value ~ctx ~arity value =
  match value with
  | { pelem = List { pelem = vs; _ }; _ } ->
      let fn acc = function
        | { pelem = List { pelem = fields; _ }; _ }
          when List.length fields = arity ->
            let* acc = acc in
            Ok (fields :: acc)
        | _ -> error_msgf "Expected a list of %d elements in %s" arity ctx in
      let* vs = List.fold_left fn (Ok []) vs in
      Ok (List.rev vs)
  | _ -> error_msgf "Expected a list for %s" ctx

let of_opamfile opamfile =
  let none = `Msg (Fmt.str "Missing %s" field_target) in
  let* target =
    let* value = Option.to_result ~none (variable field_target opamfile) in
    string_of_value ~ctx:field_target value in
  let none = `Msg (Fmt.str "Missing %s" field_dirs) in
  let* dirs = Option.to_result ~none (variable field_dirs opamfile) in
  let* dirs = entries_of_value ~ctx:field_dirs ~arity:3 dirs in
  (* The endpoints are provenance: a lock without them is still usable. *)
  let* endpoints =
    match variable field_endpoints opamfile with
    | None -> Ok []
    | Some value ->
        let* vs = entries_of_value ~ctx:field_endpoints ~arity:3 value in
        let fn acc = function
          | [ target; version; edns ] ->
              let* acc = acc in
              let* target = string_of_value ~ctx:field_endpoints target in
              let* version = string_of_value ~ctx:field_endpoints version in
              let* edns = strings_of_value ~ctx:field_endpoints edns in
              let fn acc edn =
                let* acc = acc in
                let* edn = Edn.of_string edn in
                Ok (edn :: acc) in
              let* edns = List.fold_left fn (Ok []) edns in
              let version = if version = "" then None else Some version in
              Ok ((target, (version, List.rev edns)) :: acc)
          | _ -> assert false
          (* checked by [entries_of_value] *) in
        let* vs = List.fold_left fn (Ok []) vs in
        Ok (List.rev vs) in
  let fn acc = function
    | [ url; target; checksum ] ->
        let* acc = acc in
        let* url = string_of_value ~ctx:field_dirs url in
        let* target = string_of_value ~ctx:field_dirs target in
        let* checksum = strings_of_value ~ctx:field_dirs checksum in
        let fn acc hash =
          let* acc = acc in
          match Opam.to_checksum hash with
          | Some hash -> Ok (hash :: acc)
          | None -> error_msgf "Invalid checksum %S for %s" hash target in
        let* checksum = List.fold_left fn (Ok []) checksum in
        let version, edns =
          Option.value ~default:(None, []) (List.assoc_opt target endpoints)
        in
        Ok ({ target; url; checksum = List.rev checksum; version; edns } :: acc)
    | _ -> assert false
    (* checked by [entries_of_value] *) in
  let* entries = List.fold_left fn (Ok []) dirs in
  Ok { target; entries = List.rev entries }

let load filepath =
  match OpamParser.FullPos.file (Fpath.to_string filepath) with
  | opamfile -> of_opamfile opamfile
  | exception exn ->
      error_msgf "Invalid lock file %a: %s" Fpath.pp filepath
        (Printexc.to_string exn)

let to_jobs { entries; _ } =
  let fn acc { target; url; checksum; version; edns } =
    let* acc = acc in
    match Resolve.action_of_url ~checksum ~version url with
    | Error (`Msg msg) -> error_msgf "%s: %s" target msg
    | Ok (action, edn) ->
        let edns = if edns = [] then [ edn ] else edns in
        let name = None in
        Ok (Resolve.Job { Resolve.target; edns; action; name } :: acc) in
  let* jobs = List.fold_left fn (Ok []) entries in
  Ok (List.rev jobs)

let entry_of_job ~ls_remote { Resolve.target; edns; action; _ } =
  match action with
  | Resolve.Download { uri; checksum = []; _ } ->
      error_msgf
        "%s: no checksum is known for %s, it cannot be locked (an archive \
         given verbatim in the _mfetch file has none; use the opam package \
         instead)"
        target uri
  | Download { uri; checksum; version; _ } ->
      Ok { target; url = uri; checksum; version; edns }
  | Copy { dirpath } ->
      error_msgf
        "%s: vendored by copy from %a, it cannot be locked (unpin it towards a \
         public URL)"
        target Fpath.pp dirpath
  | Clone { remote = `Local dirpath; _ } ->
      error_msgf
        "%s: pinned to the local Git repository %a, it cannot be locked (unpin \
         it towards a public URL)"
        target Fpath.pp dirpath
  | Clone { remote; branch; _ } -> begin
      let* (commit : Carton.Uid.t) = ls_remote ?branch remote in
      let commit = Ohex.encode (commit :> string) in
      match remote with
      | `HTTP uri ->
          Ok
            {
              target;
              url = Fmt.str "git+%s#%s" uri commit;
              checksum = [];
              version = None;
              edns;
            }
      | `SSH { Git.user; host; port; path } ->
          let url =
            Fmt.str "git+ssh://%s@%s%a/%s#%s" user host
              Fmt.(option (fmt ":%d"))
              port path commit in
          Log.warn (fun m ->
              m
                "%s is locked to an ssh endpoint (%s): whoever rebuilds it \
                 will need the corresponding credentials"
                target url) ;
          Ok { target; url; checksum = []; version = None; edns }
      | `Local _ -> assert false (* handled above *)
    end

let of_jobs ~target ~ls_remote items =
  let fn (entries, errors) = function
    | Resolve.Unresolved { target; msg; name } ->
        let name = Option.value ~default:target name in
        (entries, Fmt.str "%s: %s" name msg :: errors)
    | Job job ->
    match entry_of_job ~ls_remote job with
    | Ok entry -> (entry :: entries, errors)
    | Error (`Msg msg) -> (entries, msg :: errors) in
  let entries, errors = List.fold_left fn ([], []) items in
  let entries =
    let compare (a : entry) (b : entry) = String.compare a.target b.target in
    List.sort compare (List.rev entries) in
  match List.rev errors with
  | [] -> Ok { target; entries }
  | errors ->
      error_msgf "%a"
        Fmt.(list ~sep:(any "\n") (fun ppf -> Fmt.pf ppf "- %s"))
        errors

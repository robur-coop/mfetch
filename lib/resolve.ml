let ( let* ) = Result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

type action =
  | Download of {
      uri : string;
      archive : Edn.archive;
      checksum : Opam.hash list;
      version : string option;
      source : string;
    }
  | Clone of {
      remote : [ `HTTP of string | `SSH of Git.ssh | `Local of Fpath.t ];
      branch : string option;
      origin : string;
    }
  | Copy of { dirpath : Fpath.t }

let origin_of_edn = function
  | Edn.Git_http { uri; _ } -> uri
  | Git_ssh { user; host; port = None; path; _ } ->
      Fmt.str "%s@%s:%s" user host path
  | Git_ssh { user; host; port = Some port; path; _ } ->
      Fmt.str "ssh://%s@%s:%d/%s" user host port path
  | Git_local { dirpath; _ } -> Fpath.to_string dirpath
  | edn -> Edn.to_string edn

let verify_checksum ~expected checksum =
  let fn0 : type a. a Digestif.hash -> Opam.hash -> a Digestif.t option =
   fun hash (Opam.Hash (hash', value)) ->
    match (hash, hash') with
    | Digestif.SHA256, Digestif.SHA256 -> Some value
    | Digestif.SHA512, Digestif.SHA512 -> Some value
    | Digestif.MD5, Digestif.MD5 -> Some value
    | _ -> None in
  let fn1 (Opam.Hash (hash, value)) =
    match List.find_map (fn0 hash) expected with
    | None -> true
    | Some value' -> Digestif.equal hash value value' in
  List.for_all fn1 checksum

let action_of_edn ~root edn =
  match edn with
  | Edn.Local { dirpath; _ } -> Ok (Copy { dirpath })
  | Opam { name; version } -> begin
      let* uri =
        match Opam.resolve ~root ?version name with
        | Ok _ as v -> v
        | Error err -> error_msgf "%s: %a" name Opam.pp_error err in
      let source = uri.Opam.src in
      let* edn' = Edn.of_string source in
      match edn' with
      | Edn.Archive { uri = href; archive; checksum = expected; _ } ->
          let checksum = uri.Opam.checksum in
          let* () =
            if verify_checksum ~expected checksum
            then Ok ()
            else
              error_msgf
                "Checksums annonced by mfetch do not match what OPAM \
                 advertises for %s%a"
                name
                Fmt.(option (any "." ++ string))
                version in
          let version = Some uri.Opam.version in
          Ok (Download { uri = href; archive; checksum; version; source })
      | Git_http { uri = href; branch; name = _ } ->
          let remote = `HTTP href in
          let origin = origin_of_edn edn' in
          Ok (Clone { remote; branch; origin })
      | Git_ssh { user; host; port; path; branch; name = _ } ->
          let remote = `SSH { Git.user; host; port; path } in
          let origin = origin_of_edn edn' in
          Ok (Clone { remote; branch; origin })
      | Git_local { dirpath; branch; name = _ } ->
          let remote = `Local dirpath in
          let origin = origin_of_edn edn' in
          Ok (Clone { remote; branch; origin })
      | Local { dirpath; name = _ } -> Ok (Copy { dirpath })
      | Opam _ -> error_msgf "Invalid source for %s: %s" name source
    end
  | Archive { uri; archive; checksum; name = _ } ->
      Ok (Download { uri; archive; checksum; version = None; source = uri })
  | Git_http { uri; branch; name = _ } ->
      let remote = `HTTP uri in
      let origin = origin_of_edn edn in
      Ok (Clone { remote; branch; origin })
  | Git_ssh { user; host; port; path; branch; name = _ } ->
      let remote = `SSH { Git.user; host; port; path } in
      let origin = origin_of_edn edn in
      Ok (Clone { remote; branch; origin })
  | Git_local { dirpath; branch; name = _ } ->
      let remote = `Local dirpath in
      let origin = origin_of_edn edn in
      Ok (Clone { remote; branch; origin })

let action_of_url ~checksum ~version url =
  let* edn = Edn.of_string url in
  match edn with
  | Edn.Archive { uri; archive; _ } ->
      (* [Edn.of_string url] never produces a checksum. *)
      let source = uri in
      Ok (Download { uri; archive; checksum; version; source }, edn)
  | Git_http { uri; branch; _ } ->
      let action =
        Clone { remote = `HTTP uri; branch; origin = origin_of_edn edn } in
      Ok (action, edn)
  | Git_ssh { user; host; port; path; branch; _ } ->
      let remote = `SSH { Git.user; host; port; path } in
      Ok (Clone { remote; branch; origin = origin_of_edn edn }, edn)
  | Git_local _ | Local _ | Opam _ ->
      error_msgf "%S is not a source a third party can fetch" url

let key_of_action = function
  | Download { uri; _ } -> Fmt.str "archive:%s" uri
  | Copy { dirpath } -> Fmt.str "copy:%a" Fpath.pp dirpath
  | Clone { remote; branch; _ } ->
      let remote =
        match remote with
        | `HTTP uri -> uri
        | `Local dirpath -> Fmt.str "file://%a" Fpath.pp dirpath
        | `SSH { Git.user; host; port; path } ->
            Fmt.str "%s@%s%a:%s" user host Fmt.(option (fmt ":%d")) port path
      in
      Fmt.str "git:%s#%a" remote Fmt.(option string) branch

type job = {
  target : string;
  edns : Edn.t list;
  action : action;
  name : string option;
}

type entry =
  | Job of job
  | Unresolved of { target : string; msg : string; name : string option }

let coalesce ~root edns =
  let tbl = Hashtbl.create 0x7ff in
  let fn items edn =
    match action_of_edn ~root edn with
    | Error (`Msg msg) -> `Unresolved (Edn.name edn, msg) :: items
    | Ok action -> begin
        (* NOTE(dinosaure): we lint [action] to a unique key to be
           able to recognize possible doublon. *)
        let key = key_of_action action in
        match Hashtbl.find_opt tbl key with
        | Some job ->
            let edns = job.edns @ [ edn ] in
            Hashtbl.replace tbl key { job with edns } ;
            items
        | None ->
            let target = Edn.name edn in
            let value = { target; edns = [ edn ]; action; name = None } in
            Hashtbl.add tbl key value ;
            `Key key :: items
      end in
  let items = List.rev (List.fold_left fn [] edns) in
  let fn = function
    | `Key key -> Job (Hashtbl.find tbl key)
    | `Unresolved (target, msg) -> Unresolved { target; msg; name = None } in
  let items = List.map fn items in
  let module M = Map.Make (String) in
  let count =
    let fn acc = function
      | Job { target; _ } ->
          let fn v = Some (Option.value v ~default:0 + 1) in
          M.update target fn acc
      | Unresolved _ -> acc in
    List.fold_left fn M.empty items in
  let fn = function
    | Job ({ target; name; _ } as job) when M.find target count > 1 ->
        let msg =
          Fmt.str "several distinct sources for the same target (%a)"
            Fmt.(list ~sep:(any ", ") Edn.pp)
            job.edns in
        Unresolved { target; msg; name }
    | item -> item in
  List.map fn items

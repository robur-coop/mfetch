let ( let* ) = Result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

type ssh = { user : string; host : string; port : int option; path : string }

type action =
  | Download of {
      uri : string;
      archive : Mfetch.Edn.archive;
      checksum : Mfetch.Opam.hash list;
      version : string option;
      source : string;
    }
  | Clone of {
      remote : [ `HTTP of string | `SSH of ssh | `Local of Fpath.t ];
      branch : string option;
      origin : string;
    }
  | Copy of { dirpath : Fpath.t }

let origin_of_edn = function
  | Mfetch.Edn.Git_http { uri; _ } -> uri
  | Git_ssh { user; host; port = None; path; _ } ->
      Fmt.str "%s@%s:%s" user host path
  | Git_ssh { user; host; port = Some port; path; _ } ->
      Fmt.str "ssh://%s@%s:%d/%s" user host port path
  | Git_local { dirpath; _ } -> Fpath.to_string dirpath
  | edn -> Mfetch.Edn.to_string edn

let action_of_edn ~root edn =
  match edn with
  | Mfetch.Edn.Local { dirpath } -> Ok (Copy { dirpath })
  | Opam { name : string; version : string option } -> begin
      let* uri =
        match Mfetch.Opam.resolve ~root ?version name with
        | Ok _ as v -> v
        | Error err -> error_msgf "%s: %a" name Mfetch.Opam.pp_error err in
      let source = uri.Mfetch.Opam.src in
      let* edn' = Mfetch.Edn.of_string source in
      match edn' with
      | Mfetch.Edn.Archive { uri = href; archive } ->
          let checksum = uri.Mfetch.Opam.checksum in
          let version = Some uri.Mfetch.Opam.version in
          Ok (Download { uri = href; archive; checksum; version; source })
      | Git_http { uri = href; branch } ->
          let remote = `HTTP href in
          let origin = origin_of_edn edn' in
          Ok (Clone { remote; branch; origin })
      | Git_ssh { user; host; port; path; branch } ->
          let remote = `SSH { user; host; port; path } in
          let origin = origin_of_edn edn' in
          Ok (Clone { remote; branch; origin })
      | Git_local { dirpath; branch } ->
          let remote = `Local dirpath in
          let origin = origin_of_edn edn' in
          Ok (Clone { remote; branch; origin })
      | Local { dirpath } -> Ok (Copy { dirpath })
      | Opam _ -> error_msgf "Invalid source for %s: %s" name source
    end
  | Archive { uri; archive } ->
      Ok
        (Download { uri; archive; checksum = []; version = None; source = uri })
  | Git_http { uri; branch } ->
      let remote = `HTTP uri in
      let origin = origin_of_edn edn in
      Ok (Clone { remote; branch; origin })
  | Git_ssh { user; host; port; path; branch } ->
      let remote = `SSH { user; host; port; path } in
      let origin = origin_of_edn edn in
      Ok (Clone { remote; branch; origin })
  | Git_local { dirpath; branch } ->
      let remote = `Local dirpath in
      let origin = origin_of_edn edn in
      Ok (Clone { remote; branch; origin })

let key_of_action = function
  | Download { uri; _ } -> Fmt.str "archive:%s" uri
  | Copy { dirpath } -> Fmt.str "copy:%a" Fpath.pp dirpath
  | Clone { remote; branch; _ } ->
      let remote =
        match remote with
        | `HTTP uri -> uri
        | `Local dirpath -> Fmt.str "file://%a" Fpath.pp dirpath
        | `SSH { user; host; port; path } ->
            Fmt.str "%s@%s%a:%s" user host Fmt.(option (fmt ":%d")) port path
      in
      Fmt.str "git:%s#%a" remote Fmt.(option string) branch

let coalesce ~root edns =
  let tbl = Hashtbl.create 0x7ff in
  let fn items edn =
    match action_of_edn ~root edn with
    | Error (`Msg msg) -> `Unresolved (Mfetch.Edn.name edn, msg) :: items
    | Ok action -> begin
        (* NOTE(dinosaure): we lint [action] to a unique key to be
           able to recognize possible doublon. *)
        let key = key_of_action action in
        match Hashtbl.find_opt tbl key with
        | Some _ -> items
        | None ->
            let value = { target; edns = [ edn ]; action } in
            Hashtbl.add tbl key value ;
            `Key key :: items
      end in
  let items = List.rev (List.fold_left fn [] edns) in
  let fn = function
    | `Key key -> Job (Hashtbl.find tbl key)
    | `Unresolved (target, err) -> Unresolved { target; err } in
  let items = List.map fn items in
  assert false

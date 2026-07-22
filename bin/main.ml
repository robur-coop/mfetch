module Prgrss = struct
  type t = {
    add : string -> (int -> unit) * (unit -> unit);
    finally : unit -> unit;
  }

  let make ~config = function
    | false -> { add = (fun _ -> (ignore, ignore)); finally = ignore }
    | true ->
        let display = Progress.Display.start ~config Progress.Multi.blank in
        let previous = !Mfetch_cli.interject in
        Mfetch_cli.interject :=
          { Mfetch_cli.run = (fun fn -> Progress.interject_with fn) } ;
        let add name =
          let line =
            let open Progress.Line in
            constf " %-24s " name
            ++ spinner ()
            ++ const " "
            ++ bytes
            ++ const " "
            ++ bytes_per_sec in
          let reporter = Progress.Display.add_line display line in
          let fn len = Progress.Reporter.report reporter len in
          let finally () = Progress.Reporter.finalise reporter in
          (fn, finally) in
        let finally () =
          Progress.Display.finalise display ;
          Mfetch_cli.interject := previous in
        { add; finally }
end

let ( let* ) = Result.bind
let ( let@ ) finally fn = Fun.protect ~finally fn
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

type action =
  | Download of {
      uri : string;
      archive : Mfetch.Edn.archive;
      checksum : Mfetch.Opam.hash list;
      version : string option;
      source : string;
    }
  | Clone of {
      remote : [ `HTTP of string | `SSH of Mfetch.Git.ssh | `Local of Fpath.t ];
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
          let remote = `SSH { Mfetch.Git.user; host; port; path } in
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
      let remote = `SSH { Mfetch.Git.user; host; port; path } in
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

type job = { target : string; edns : Mfetch.Edn.t list; action : action }
type entry = Job of job | Unresolved of { target : string; err : string }

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
        | Some job ->
            let edns = job.edns @ [ edn ] in
            Hashtbl.replace tbl key { job with edns } ;
            items
        | None ->
            let target = Mfetch.Edn.name edn in
            let value = { target; edns = [ edn ]; action } in
            Hashtbl.add tbl key value ;
            `Key key :: items
      end in
  let items = List.rev (List.fold_left fn [] edns) in
  let fn = function
    | `Key key -> Job (Hashtbl.find tbl key)
    | `Unresolved (target, err) -> Unresolved { target; err } in
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
    | Job ({ target; _ } as job) when M.find target count > 1 ->
        let err =
          Fmt.str "several distinct sources for the same target (%a)"
            Fmt.(list ~sep:(any ", ") Mfetch.Edn.pp)
            job.edns in
        Unresolved { target; err }
    | item -> item in
  List.map fn items

type outcome =
  | Fetched of Mfetch.State.entry
  | Skipped of { entry : Mfetch.State.entry option; msg : string }

let check_dune_project into =
  if Sys.file_exists Fpath.(to_string (into / "dune-project"))
  then Ok ()
  else
    error_msgf "%a does not contain a dune-project file (kept for inspection)"
      Fpath.pp into

let unable_to_delete filepath =
  match Bos.OS.Dir.delete ~recurse:true filepath with
  | Ok () -> false
  | Error _ -> true

let process ~force ~resolver ~progress ~dst previous { target; edns; action } =
  let into = Fpath.(dst / target) in
  let exists = Sys.file_exists (Fpath.to_string into) in
  if exists && not force
  then begin
    let fn entry = entry.Mfetch.State.target = target in
    let entry = List.find_opt fn previous in
    let fn entry = { entry with Mfetch.State.edns } in
    let entry = Option.map fn entry in
    let msg =
      match entry with
      | Some _ -> "already vendored"
      | None -> "already exists (unknown to mfetch, delete it to fetch)" in
    Ok (Skipped { entry; msg })
  end
  else if exists && unable_to_delete into
  then error_msgf "Unable to delete the previous version"
  else begin
    let reporter, finally = progress.Prgrss.add target in
    let@ () = finally in
    let* source, version, k, commit =
      match action with
      | Download { uri; archive; checksum; version; source } ->
          let* () =
            Mfetch.Archive.download ~resolver ~checksum ~reporter
              (uri, archive, into) in
          Ok (Some source, version, `Archive, None)
      | Clone { remote; branch; origin } ->
          let* head =
            Mfetch.Git.clone ~resolver ~remote ?branch ~reporter ~origin into
          in
          Ok (Some origin, None, `Git, Some head)
      | Copy { dirpath = src } ->
          let* () = Mfetch.Archive.copy ~reporter ~src into in
          let source = Fmt.str "file://%a" Fpath.pp src in
          Ok (Some source, None, `Archive, None) in
    let* entry =
      let* () = check_dune_project into in
      let* hash = Mfetch.State.hash_of_tree into in
      Ok { Mfetch.State.target; edns; source; version; k; commit; hash } in
    Ok (Fetched entry)
  end

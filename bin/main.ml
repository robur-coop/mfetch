let ( let* ) = Result.bind
let ( let@ ) finally fn = Fun.protect ~finally fn
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

open Mfetch.Resolve

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

let process ~force ~resolver ?authenticator ~progress ~dst previous
    { target; edns; action } =
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
    let { Prgrss.report = reporter; total = on_total; finally } =
      progress.Prgrss.add target in
    let@ () = finally in
    let* source, version, k, commit =
      match action with
      | Download { uri; archive; checksum; version; source } ->
          let* () =
            Mfetch.Archive.download ~resolver ?authenticator ~checksum ~reporter
              ~on_total (uri, archive, into) in
          Ok (Some source, version, `Archive, None)
      | Clone { remote; branch; origin } ->
          let* head =
            Mfetch.Git.clone ~resolver ?authenticator ~remote ?branch ~reporter
              ~origin into in
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

let update_dune_file ~target:_ = assert false

let pp_status ppf = function
  | `Ok -> Fmt.(styled `Green string) ppf "ok"
  | `Skipped -> Fmt.(styled `Yellow string) ppf "skipped"
  | `Failed -> Fmt.(styled `Red string) ppf "failed"
  | `Modified -> Fmt.(styled `Yellow string) ppf "modified"
  | `Missing -> Fmt.(styled `Red string) ppf "missing"
  | `Not_fetched -> Fmt.(styled `Yellow string) ppf "not fetched"

let show_results results =
  let pp_merged ppf = function
    | [] | [ _ ] -> ()
    | edns -> Fmt.pf ppf " (%a)" Fmt.(list ~sep:(any ", ") Mfetch.Edn.pp) edns
  in
  let fn (target, edns, outcome) =
    match outcome with
    | Ok (Fetched _) ->
        Fmt.pr "%-32s %a%a\n%!" target pp_status `Ok pp_merged edns
    | Ok (Skipped { msg; _ }) ->
        Fmt.pr "%-32s %a (%s)%a\n%!" target pp_status `Skipped msg pp_merged
          edns
    | Error err ->
        Fmt.pr "%-32s %a: %a\n%!" target pp_status `Failed Mfetch.Git.pp_error
          err in
  List.iter fn results

let run_fetch quiet root filepath target with_dune_file force jobs
    no_progress (config, columns) authenticator =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  Mirage_crypto_rng_unix.use_default () ;
  Miou_unix.run @@ fun () ->
  let result =
    let* edns = Mfetch.Edn.from_filepath filepath in
    let daemon, happy_eyeballs = Happy_eyeballs_miou_unix.create () in
    let@ () = fun () -> Happy_eyeballs_miou_unix.kill daemon in
    let resolver = `Happy happy_eyeballs in
    let state = Fpath.(target / ".mfetch.state") in
    let* previous = Mfetch.State.load state in
    let items = coalesce ~root edns in
    let progress =
      Prgrss.make ~config ~columns
        ((not no_progress) && (not quiet) && Unix.isatty Unix.stderr) in
    let@ () = progress.Prgrss.finally in
    let rem =
      List.filter_map (function Job job -> Some job | _ -> None) items in
    let q = Flux.Bqueue.(create with_close) (List.length rem + 1) in
    List.iter (Flux.Bqueue.put q) rem ;
    Flux.Bqueue.close q ;
    let worker () =
      let rec go acc =
        match Flux.Bqueue.get q with
        | None -> List.rev acc
        | Some job ->
            let v =
              process ~force ~resolver ?authenticator ~progress ~dst:target
                previous job in
            go ((job.target, v) :: acc) in
      go [] in
    let results : (string * (outcome, [> Mfetch.Git.error ]) result) list =
      Miou.parallel worker (List.init (Int.max 1 jobs) (Fun.const ()))
      |> List.concat_map @@ function
         | Ok results -> results
         | Error exn -> raise exn in
    let fn = function
      | Unresolved { target; msg } -> (target, [], Error (`Msg msg))
      | Job job ->
          let _, outcome = List.find (fun (t, _) -> t = job.target) results in
          (job.target, job.edns, outcome) in
    let results = List.map fn items in
    progress.Prgrss.finally () ;
    if not quiet then show_results results ;
    let fn (_, _, outcome) =
      match outcome with
      | Ok (Fetched entry | Skipped { entry = Some entry; _ }) -> Some entry
      | Ok (Skipped { entry = None; _ }) -> None
      | Error _ -> None in
    let entries = List.filter_map fn results in
    let* () =
      if entries <> [] || Sys.file_exists (Fpath.to_string state)
      then begin
        let* _ = Bos.OS.Dir.create ~path:true target in
        let lock =
          Mfetch.Opam.Lock.flock `Lock_write
            Fpath.(target / ".mfetch.state.lock") in
        let@ () = fun () -> Mfetch.Opam.Lock.update `Lock_none lock in
        Mfetch.State.save state entries
      end
      else Ok () in
    let* () = if with_dune_file then update_dune_file ~target else Ok () in
    let failed =
      let fn = function _, _, Error _ -> true | _ -> false in
      List.exists fn results in
    Ok (if failed then 1 else 0) in
  match result with
  | Ok _ -> 0
  | Error (`Msg msg) ->
      Logs.err (fun m -> m "%s" msg) ;
      2

open Cmdliner
open Mfetch_cli

let term =
  let open Term in
  const run_fetch
  $ setup_logs
  $ setup_opam_root
  $ file
  $ target
  $ with_dune_file
  $ force
  $ jobs
  $ no_progress
  $ setup_progress
  $ setup_authenticator

let fetch =
  let doc = "Fetch and vendor the sources listed in the $(b,_mfetch) file." in
  Cmd.v (Cmd.info "fetch" ~doc) term

let cmd =
  let doc = "A tool to fetch and vendor sources of OPAM packages." in
  let man = [] in
  let info = Cmd.info "mfetch" ~doc ~man in
  Cmd.group ~default:term info [ fetch ]

let () = exit (Cmd.eval' cmd)

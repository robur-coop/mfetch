let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

open Cmdliner

let output_options = "OUTPUT OPTIONS"

let verbosity =
  let env = Cmd.Env.info "MFETCH_LOGS" in
  Logs_cli.level ~docs:output_options ~env ()

let renderer =
  let env = Cmd.Env.info "MFETCH_FMT" in
  Fmt_cli.style_renderer ~docs:output_options ~env ()

let utf_8 =
  let doc = "Allow binaries to emit UTF-8 characters." in
  let env = Cmd.Env.info "MFETCH_UTF_8" in
  Arg.(value & opt bool true & info [ "with-utf-8" ] ~doc ~env)

type interject = { run : 'a. (unit -> 'a) -> 'a }

let interject = ref { run = (fun fn -> fn ()) }

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over () ;
      k () in
    let with_metadata header _tags k ppf fmt =
      Fmt.kpf k ppf
        ("[%a]%a[%a]: " ^^ fmt ^^ "\n%!")
        Fmt.(styled `Cyan int)
        (Stdlib.Domain.self () :> int)
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src) in
    !interject.run @@ fun () ->
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt in
  { Logs.report }

let setup_logs utf_8 style_renderer level =
  Fmt_tty.setup_std_outputs ~utf_8 ?style_renderer () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter Fmt.stderr) ;
  Option.is_none level

let setup_logs = Term.(const setup_logs $ utf_8 $ renderer $ verbosity)

let existing_directory =
  let parser str =
    match Fpath.of_string str with
    | Ok v when Sys.file_exists str && Sys.is_directory str ->
        Ok (Fpath.to_dir_path v)
    | Ok v ->
        error_msgf "%a does not exist or it's not a valid directory" Fpath.pp v
    | Error _ as err -> err in
  Arg.conv (parser, Fpath.pp)

let setup_opam_root home opam_root =
  match (home, opam_root) with
  | Some home, None -> Ok Fpath.(home / ".opam")
  | _, Some root -> Ok root
  | None, None -> error_msgf "Impossible to infer the current OPAM root path"

let home =
  let doc = "The current user's home directory." in
  let env = Cmd.Env.info "HOME" in
  let open Arg in
  value & opt (some existing_directory) None & info [ "home" ] ~doc ~env

let root =
  let doc = "Use $(i,ROOT) as the current OPAM root path." in
  let env = Cmd.Env.info "OPAMROOT" in
  let open Arg in
  value
  & opt (some existing_directory) None
  & info [ "root" ] ~doc ~docv:"ROOT" ~env

let setup_opam_root = Term.(const setup_opam_root $ home $ root)
let fpath = Arg.conv (Fpath.of_string, Fpath.pp)

let file =
  let doc = "The file listing the packages to vendor (bcfg format)." in
  let open Arg in
  value & opt fpath (Fpath.v "_mfetch") & info [ "f"; "file" ] ~doc ~docv:"FILE"

let target =
  let doc = "The directory into which sources are vendored." in
  let open Arg in
  value
  & opt fpath (Fpath.v "vendors/")
  & info [ "target" ] ~doc ~docv:"DIRECTORY"

let with_dune_file =
  let doc =
    "Add a (vendored_dirs ...) stanza to the root dune file (created if it \
     does not exist)." in
  Arg.(value & flag & info [ "with-dune-file" ] ~doc)

let force =
  let doc =
    "Delete and fetch again the packages already vendored (local modifications \
     are lost)." in
  Arg.(value & flag & info [ "force" ] ~doc)

let jobs =
  let doc = "Number of parallel downloads." in
  Arg.(value & opt int 4 & info [ "j"; "jobs" ] ~doc ~docv:"JOBS")

let no_progress =
  let doc = "Don't display progress bars." in
  Arg.(value & flag & info [ "no-progress" ] ~doc)

let setup_progress max_width =
  let config = Progress.Config.v ~max_width () in
  (config, Option.value ~default:80 max_width)

let width =
  let doc = "Width of the terminal." in
  let default = Terminal.Size.get_columns () in
  let open Arg in
  value & opt (some int) default & info [ "width" ] ~doc ~docv:"WIDTH"

let setup_progress = Term.(const setup_progress $ width)

let setup_authenticator () =
  match Ca_certs.authenticator () with
  | Ok authenticator -> Some authenticator
  | Error (`Msg msg) ->
      Logs.warn (fun m -> m "Unable to load the system's CA store: %s" msg) ;
      None

let setup_authenticator = Term.(const setup_authenticator $ const ())

let src = Logs.Src.create "mfetch.opam"

module Log = (val Logs.src_log src : Logs.LOG)

let failwith_error_msg = function Ok v -> v | Error (`Msg msg) -> failwith msg
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt
let ( let@ ) finally fn = Fun.protect ~finally fn
let ( let* ) = Result.bind

module Lock = struct
  type lock_rdwr = [ `Lock_read | `Lock_write ]
  type lock = [ `Lock_none | lock_rdwr ]

  let pp_lock ppf = function
    | `Lock_none -> Fmt.string ppf "none"
    | `Lock_read -> Fmt.string ppf "read"
    | `Lock_write -> Fmt.string ppf "write"

  type t = {
    mutable flag : lock;
    filepath : Fpath.t;
    mutable fd : Unix.file_descr option;
  }

  let locks = Hashtbl.create 16

  let release_all_locks () =
    Hashtbl.iter (fun fd _ -> Unix.close fd) locks ;
    Hashtbl.clear locks

  let () = at_exit release_all_locks

  let unix_flag_from_lock ?(block = true) = function
    | `Lock_read -> if block then Unix.F_RLOCK else Unix.F_TRLOCK
    | `Lock_write -> if block then Unix.F_LOCK else Unix.F_TLOCK

  let rec update want lock =
    if want <> lock.flag
    then
      match (want, lock) with
      | `Lock_none, { fd = Some fd; flag = #lock_rdwr as flag; filepath } ->
          Log.debug (fun m ->
              m "unlock %a (%a => %a)" Fpath.pp filepath pp_lock flag pp_lock
                `Lock_none) ;
          Hashtbl.remove locks fd ;
          Unix.close fd ;
          lock.flag <- `Lock_none ;
          lock.fd <- None
      | (#lock_rdwr as want), { fd = None; flag = `Lock_none; filepath } ->
          Log.debug (fun m ->
              m "flock %a (%a => %a)" Fpath.pp filepath pp_lock `Lock_none
                pp_lock want) ;
          let lock' = flock want filepath in
          lock.flag <- want ;
          lock.fd <- lock'.fd
      | `Lock_write, { fd = Some fd; filepath; flag = `Lock_read } ->
          Log.debug (fun m ->
              m "flock %a (%a => %a)" Fpath.pp filepath pp_lock `Lock_write
                pp_lock `Lock_read) ;
          Unix.close fd ;
          let lock' = flock want filepath in
          lock.flag <- `Lock_write ;
          lock.fd <- lock'.fd
      | (#lock_rdwr as want), { fd = Some fd; flag; filepath } ->
          Log.debug (fun m ->
              m "lockf %a (%a => %a)" Fpath.pp filepath pp_lock flag pp_lock
                want) ;
          if flag <> want
          then
            begin try
              if Sys.win32 && flag <> `Lock_none
              then Unix.lockf fd Unix.F_ULOCK 0 ;
              Unix.lockf fd (unix_flag_from_lock ~block:false want) 0
            with
            | Unix.Unix_error (Unix.EAGAIN, _, _)
            | Unix.Unix_error (Unix.EACCES, _, _)
            ->
              Log.info (fun m ->
                  m "Another process has locked %a, waiting (C-c to abort)..."
                    Fpath.pp filepath) ;
              let rec go () =
                try Unix.lockf fd (unix_flag_from_lock ~block:true want) 0 with
                | Sys.Break as exn -> raise exn
                | Unix.Unix_error (Unix.EINTR, _, _) -> go () in
              go ()
            end ;
          lock.flag <- want
      | _ -> assert false

  and flock flag filepath =
    match flag with
    | `Lock_none -> { fd = None; filepath; flag = `Lock_none }
    | #lock_rdwr as flag ->
        let r = Bos.OS.Dir.create (Fpath.parent filepath) in
        let _ = failwith_error_msg r in
        let uflag =
          match flag with
          | `Lock_write -> Unix.O_RDWR
          | `Lock_read -> Unix.O_RDONLY in
        let fd =
          Unix.openfile (Fpath.to_string filepath)
            Unix.[ O_CREAT; O_CLOEXEC; O_SHARE_DELETE; uflag ]
            0o666 in
        Hashtbl.replace locks fd () ;
        let current = { fd = Some fd; filepath; flag = `Lock_none } in
        Log.debug (fun m -> m "=> %a" pp_lock flag) ;
        update flag current ;
        current
end

type error = [ `Not_found | `Msg of string ]

let pp_error ppf = function
  | `Not_found -> Fmt.string ppf "Not found"
  | `Msg msg -> Fmt.pf ppf "%s" msg

type hash = Hash : 'a Digestif.hash * 'a Digestif.t -> hash

type uri = {
  package : string;
  version : string;
  src : string;
  checksum : hash list;
}

let pp_hash ppf (Hash (k, v)) = Digestif.pp k ppf v

let pp_src ppf { package; version; src; checksum } =
  Fmt.pf ppf
    "@[<2>{ package=@ %s;@ version=@ %s;@ src=@ %s;@ checksum=@ @[<hov>%a@]; \
     }@]"
    package version src
    Fmt.(Dump.list pp_hash)
    checksum

let without_pos { OpamParserTypes.FullPos.pelem; _ } = pelem

let get_current_switch { OpamParserTypes.FullPos.file_contents; _ } =
  let fn { OpamParserTypes.FullPos.pelem; _ } =
    match pelem with
    | OpamParserTypes.FullPos.Variable
        ({ pelem = "switch"; _ }, { pelem = String switch; _ }) ->
        Some switch
    | _ -> None in
  List.find_map fn file_contents |> Option.to_result ~none:`Not_found

let to_checksum str =
  let ( let* ) = Option.bind in
  let str = String.lowercase_ascii str in
  match String.split_on_char '=' str with
  | [ "sha512"; v ] ->
      let* hash = Digestif.of_hex_opt Digestif.SHA512 v in
      Some (Hash (Digestif.SHA512, hash))
  | [ "sha256"; v ] ->
      let* hash = Digestif.of_hex_opt Digestif.SHA256 v in
      Some (Hash (Digestif.SHA256, hash))
  | [ "md5"; v ] ->
      let* hash = Digestif.of_hex_opt Digestif.MD5 v in
      Some (Hash (Digestif.MD5, hash))
  | _ -> None

let string_of_hash (Hash (k, v)) =
  let kind =
    match k with
    | Digestif.SHA512 -> "sha512"
    | Digestif.SHA256 -> "sha256"
    | Digestif.MD5 -> "md5"
    | _ -> Fmt.invalid_arg "Unsupported checksum kind" in
  Fmt.str "%s=%s" kind (Digestif.to_hex k v)

let get_package_and_version str =
  let vs = String.split_on_char '.' str in
  (List.hd vs, String.concat "." (List.tl vs))

let get_url_section ~package ~version
    { OpamParserTypes.FullPos.file_contents; _ } =
  let ( let* ) = Option.bind in
  let fn { OpamParserTypes.FullPos.pelem; _ } =
    match pelem with
    | OpamParserTypes.FullPos.Section
        { section_kind = { pelem = "url"; _ }; section_items; _ } ->
        Some section_items.pelem
    | _ -> None in
  let* url = List.find_map fn file_contents in
  let fn { OpamParserTypes.FullPos.pelem; _ } =
    match pelem with
    | OpamParserTypes.FullPos.Variable
        ({ pelem = "src"; _ }, { pelem = String src; _ }) ->
        Some src
    | _ -> None in
  let* src = List.find_map fn url in
  let fn { OpamParserTypes.FullPos.pelem; _ } =
    match pelem with
    | OpamParserTypes.FullPos.Variable
        ({ pelem = "checksum"; _ }, { pelem = List { pelem = lst; _ }; _ }) ->
        let lst = List.map without_pos lst in
        let fn = function
          | OpamParserTypes.FullPos.String str -> to_checksum str
          | _ -> None in
        let lst = List.filter_map fn lst in
        Some lst
    | OpamParserTypes.FullPos.Variable
        ({ pelem = "checksum"; _ }, { pelem = String str; _ }) ->
        Some (Option.to_list (to_checksum str))
    | _ -> None in
  (* NOTE(dinosaure): pinned packages have no checksum *)
  let checksum = Option.value ~default:[] (List.find_map fn url) in
  Some { package; version; src; checksum }

let uri_from_opam filepath =
  Log.debug (fun m -> m "Analyze %a" Fpath.pp filepath) ;
  let name = Fpath.(basename (parent filepath)) in
  let package, version = get_package_and_version name in
  match OpamParser.FullPos.file (Fpath.to_string filepath) with
  | opam ->
      let none =
        msgf "%s.%s has no url section (virtual or pinned package)" package
          version in
      get_url_section ~package ~version opam |> Option.to_result ~none
  | exception exn ->
      error_msgf "Invalid OPAM file %a (exception: %s)" Fpath.pp filepath
        (Printexc.to_string exn)

let current_switch ~root =
  let filepath = Fpath.(root / "config.lock") in
  let lock = Lock.flock `Lock_read filepath in
  let@ () = fun () -> Lock.update `Lock_none lock in
  match OpamParser.FullPos.file Fpath.(to_string (root / "config")) with
  | cfg -> get_current_switch cfg
  | exception _ -> Error `Not_found

(* [<root>/<switch>/.opam-switch/packages/] contains one directory
   [<name>.<version>] per installed package. *)
let resolve_from_switch ~root ?version name =
  let* switch = current_switch ~root in
  Log.debug (fun m -> m "switch: %s" switch) ;
  let packages = Fpath.(root / switch / ".opam-switch" / "packages") in
  let* dir =
    match version with
    | Some version ->
        let dir = Fpath.(packages / (name ^ "." ^ version)) in
        let* exists = Bos.OS.Dir.exists dir in
        if exists then Ok dir else Error `Not_found
    | None -> begin
        let fn = Fun.const `Not_found in
        let* entries = Result.map_error fn (Bos.OS.Dir.contents packages) in
        let prefix = name ^ "." in
        let fn path = String.starts_with ~prefix (Fpath.basename path) in
        match List.filter fn entries with
        | [ dir ] -> Ok dir
        | [] -> Error `Not_found
        | dir :: _ as dirs ->
            Log.warn (fun m ->
                m "%d versions of %s installed in the switch %s"
                  (List.length dirs) name switch) ;
            Ok dir
      end in
  uri_from_opam Fpath.(dir / "opam")

(* [<root>/repo/default/packages/<name>/] contains one directory
   [<name>.<version>] per available version. *)
let resolve_from_repo ~root ?version name =
  let packages = Fpath.(root / "repo" / "default" / "packages" / name) in
  let* dir =
    match version with
    | Some version ->
        let dir = Fpath.(packages / (name ^ "." ^ version)) in
        let* exists = Bos.OS.Dir.exists dir in
        if exists then Ok dir else Error `Not_found
    | None -> begin
        let fn = Fun.const `Not_found in
        let* entries = Result.map_error fn (Bos.OS.Dir.contents packages) in
        let version_of path =
          snd (get_package_and_version (Fpath.basename path)) in
        let fn a b = Version.compare (version_of a) (version_of b) in
        let entries = List.sort fn entries in
        match List.rev entries with
        | dir :: _ -> Ok dir
        | [] -> Error `Not_found
      end in
  uri_from_opam Fpath.(dir / "opam")

let resolve ~root ?version name =
  match version with
  | None ->
      begin match resolve_from_switch ~root name with
      | Ok url -> Ok url
      | Error err -> begin
          Log.debug (fun m ->
              m
                "%s not resolved from the current switch (%a), trying the \
                 default repository"
                name pp_error err) ;
          match resolve_from_repo ~root name with
          | Ok url -> Ok url
          | Error `Not_found -> error_msgf "Package %s not found" name
          | Error _ as err -> err
        end
      end
  | Some version ->
  match resolve_from_repo ~root ~version name with
  | Ok url -> Ok url
  | Error err -> begin
      Log.debug (fun m ->
          m
            "%s.%s not resolved from the default repository (%a), trying the \
             current switch"
            name version pp_error err) ;
      match resolve_from_switch ~root ~version name with
      | Ok url -> Ok url
      | Error `Not_found -> error_msgf "Package %s.%s not found" name version
      | Error _ as err -> err
    end

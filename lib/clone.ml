let src = Logs.Src.create "mfetch.git.clone"
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind
let inhibit fn = try fn () with _exn -> ()

let chop ~prefix str =
  if String.starts_with ~prefix str
  then
    let off = String.length prefix in
    let len = String.length str - String.length prefix in
    String.sub str off len
  else str

let is_hex str =
  let fn = function
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' -> true
    | _ -> false in
  String.length str = 40 && String.for_all fn str

module Log = (val Logs.src_log src : Logs.LOG)

type error = [ Smart.error | `Msg of string ]

let pp_error ppf = function
  | #Smart.error as err -> Smart.pp_error ppf err
  | `Msg msg -> Fmt.string ppf msg

let want refs = function
  | None ->
      let branch =
        match refs.Smart.head_symref with
        | Some symref when String.starts_with ~prefix:"refs/heads/" symref ->
            String.sub symref 11 (String.length symref - 11)
        | _ -> "main" in
      Ok (refs.Smart.head, `Branch branch)
  | Some oid when is_hex oid -> Ok (oid, `Detached)
  | Some branch -> begin
      let candidates =
        if String.starts_with ~prefix:"refs/heads/" branch
        then [ (branch, `Branch (chop ~prefix:"refs/heads/" branch)) ]
        else if String.starts_with ~prefix:"refs/tags/" branch
        then [ (branch, `Tag (chop ~prefix:"refs/tags/" branch)) ]
        else
          [
            ("refs/heads/" ^ branch, `Branch branch);
            ("refs/tags/" ^ branch, `Tag branch);
          ] in
      let fn (name, k) =
        Option.map (fun oid -> (oid, k)) (List.assoc_opt name refs.Smart.refs)
      in
      match List.find_map fn candidates with
      | Some (oid, refname) -> Ok (oid, refname)
      | None -> error_msgf "Branch or tag %S not found" branch
    end

(* through ssh or git-upload-pack *)
let rec through (ic, oc) = function
  | Protocol.Error err -> Result.Error err
  | Return v -> Ok v
  | Read { k; buffer; off; len } ->
      let len = Stdlib.input ic buffer off len in
      let res = if len = 0 then `End else `Len len in
      through (ic, oc) (k res)
  | Write { k; buffer; off; len } ->
      Stdlib.output_substring oc buffer off len ;
      Stdlib.flush oc ;
      through (ic, oc) (k len)

(* collect for http *)
let rec collect buf = function
  | Protocol.Write { k; buffer; off; len } ->
      Buffer.add_substring buf buffer off len ;
      collect buf (k len)
  | state -> state

let rec unroll (q, rem) = function
  | Protocol.Error err -> Result.Error err
  | Return v -> Ok v
  | Write _ -> assert false
  | Read { k; buffer; off; len } -> begin
      let chunk = if rem = "" then Some rem else Flux.Bqueue.get q in
      match chunk with
      | None -> unroll (q, rem) (k `End)
      | Some str ->
          let len = Int.min len (String.length str) in
          Bytes.blit_string str 0 buffer off len ;
          let rem = String.sub str len (String.length str - len) in
          unroll (q, rem) (k (`Len len))
    end

let is_redirection (resp : Httpcats.response) =
  H2.Status.is_redirection resp.Httpcats.status

let request ~resolver ?(meth = `GET) ?(headers = []) ?body uri state =
  let q = Flux.Bqueue.(create with_close_and_halt) 0x7ff in
  let push str = inhibit @@ fun () -> Flux.Bqueue.put q str in
  let fn _meta _req resp () = function
    | Some str when is_redirection resp -> push str
    | _ -> () in
  let producer =
    Miou.async @@ fun () ->
    let body = Option.map (fun str -> Httpcats.String str) body in
    let result =
      Httpcats.request ~resolver ~follow_redirect:true ~meth ~headers ?body ~fn
        ~uri () in
    let () = inhibit @@ fun () -> Flux.Bqueue.close q in
    result in
  let result = unroll (q, "") state in
  Flux.Bqueue.halt q ;
  let status = Miou.await producer in
  match (result, status) with
  | (Error _ as err), _ -> err
  | Ok v, Ok (Ok (resp, ())) when resp.Httpcats.status = `OK -> Ok v
  | Ok _, Ok (Ok (resp, ())) ->
      error_msgf "Unexpected response %a" H2.Status.pp_hum resp.Httpcats.status
  | Ok _, Ok (Error err) -> error_msgf "%s: %a" uri Httpcats.pp_error err
  | Ok _, Error exn -> error_msgf "%s: %s" uri (Printexc.to_string exn)

let fetch_through_http ~resolver uri ?branch q =
  let headers = [ ("Git-Protocol", "version=2") ] in
  let* _capabilities =
    let ctx = Protocol.ctx () in
    request ~resolver ~headers
      (uri ^ "/info/refs?service=git-upload-pack")
      (Smart.advertisement ctx) in
  let headers =
    [
      ("Content-Type", "application/x-git-upload-pack-request");
      ("Accept", "application/x-git-upload-pack-result");
      ("Git-Protocol", "version=2");
    ] in
  let uri = uri ^ "/git-upload-pack" in
  let post state =
    let buf = Buffer.create 0x7ff in
    let state = collect buf state in
    let body = Buffer.contents buf in
    request ~resolver ~meth:`POST ~headers ~body uri state in
  let* refs = post (Smart.ls_refs (Protocol.ctx ())) in
  let* want, refname = want refs branch in
  let* errored = post (Smart.fetch ~want q (Protocol.ctx ())) in
  if errored
  then error_msgf "%s: remote error during fetch" uri
  else Ok (want, refname)

let fetch_with_process ~cmd ?branch q =
  let ctx = Protocol.ctx () in
  let smart =
    let ( let* ) = Protocol.bind in
    let* _capabilities = Smart.advertisement ctx in
    let* refs = Smart.ls_refs ctx in
    match want refs branch with
    | Error (`Msg msg) -> Protocol.error (`Msg msg)
    | Ok (want, refname) ->
        let* errored = Smart.fetch ~want q ctx in
        Protocol.return ((want, refname), errored) in
  let ic, oc = Unix.open_process cmd in
  let result = through (ic, oc) smart in
  let status = Unix.close_process (ic, oc) in
  match (result, status) with
  | Ok ((refname, oid), false), Unix.WEXITED 0 -> Ok (refname, oid)
  | Ok (_, true), _ -> error_msgf "%s: remote error during fetch" cmd
  | Ok _, Unix.WEXITED n -> error_msgf "%s existed with %d" cmd n
  | Ok _, Unix.(WSIGNALED _ | WSTOPPED _) -> error_msgf "%s killed" cmd
  | Error err, _ -> Error err

let fetch_through_ssh ~user ~host ?(port = 22) ~path ?branch q =
  let remote = Fmt.str "%s@%s" user host in
  let cmd = Fmt.str "git-upload-pack '%s'" path in
  let cmd =
    Fmt.str "GIT_PROTOCOL=version=2 ssh -o SendEnv=GIT_PROTOCOL -p %d %s %s"
      port remote (Filename.quote cmd) in
  fetch_with_process ~cmd ?branch q

let fetch_local_git_repository dirpath ?branch q =
  let cmd =
    Fmt.str "GIT_PROTOCOL=version=2 git-upload-pack %s"
      (Filename.quote (Fpath.to_string dirpath)) in
  fetch_with_process ~cmd ?branch q

let ( let@ ) finally fn = Fun.protect ~finally fn
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

type entry = {
  target : string;
  edns : Edn.t list;
  source : string option;
  version : string option;
  k : [ `Archive | `Git ];
  commit : Carton.Uid.t option;
  hash : Digestif.SHA256.t;
}

type t = entry list

let hash_of_tree root =
  let ctx = ref (Digestif.SHA256.init ()) in
  let feed str = ctx := Digestif.SHA256.feed_string !ctx str in
  let feed_file filepath =
    let ic = In_channel.open_bin (Fpath.to_string filepath) in
    let@ () = fun () -> In_channel.close ic in
    let buf = Bytes.create 0x7ff in
    let rec go () =
      let len = In_channel.input ic buf 0 (Bytes.length buf) in
      if len > 0
      then begin
        ctx := Digestif.SHA256.feed_bytes !ctx ~off:0 ~len buf ;
        go ()
      end in
    go () in
  let rec walk relpath dirpath =
    let entries = Sys.readdir (Fpath.to_string dirpath) in
    Array.sort String.compare entries ;
    let fn entry =
      if entry <> ".git"
      then begin
        let relpath =
          if relpath = "" then entry else Filename.concat relpath entry in
        let filepath = Fpath.(dirpath / entry) in
        let stat = Unix.lstat (Fpath.to_string filepath) in
        match stat.Unix.st_kind with
        | Unix.S_DIR -> walk relpath (Fpath.to_dir_path filepath)
        | Unix.S_LNK ->
            let value = Unix.readlink (Fpath.to_string filepath) in
            let str =
              Fmt.str "%s\x00l\x00%d\x00%s" relpath (String.length value) value
            in
            feed str
        | Unix.S_REG ->
            let exec = stat.Unix.st_perm land 0o100 <> 0 in
            let str =
              Fmt.str "%s\x00f%s\x00%d\x00" relpath
                (if exec then "x" else "")
                stat.Unix.st_size in
            feed str ;
            feed_file filepath
        | _ -> ()
      end in
    Array.iter fn entries in
  match walk "" root with
  | () -> Ok (Digestif.SHA256.get !ctx)
  | exception Sys_error err -> error_msgf "%s" err
  | exception Unix.Unix_error (err, _, _) ->
      error_msgf "%a: %s" Fpath.pp root (Unix.error_message err)

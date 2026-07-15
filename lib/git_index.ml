type entry = { path : Fpath.t; mode : int; uid : Carton.Uid.t }

let add_u16 buf v = Buffer.add_uint16_be buf (v land 0xffff)
let add_u32 buf v = Buffer.add_int32_be buf (Int32.of_int (v land 0xffffffff))

let add_entry buf ~worktree { path; mode; uid } =
  let stat = Unix.lstat Fpath.(to_string (worktree // path)) in
  let start = Buffer.length buf in
  add_u32 buf (int_of_float stat.Unix.st_ctime) ;
  add_u32 buf 0 ;
  add_u32 buf (int_of_float stat.Unix.st_mtime) ;
  add_u32 buf 0 ;
  add_u32 buf stat.Unix.st_dev ;
  add_u32 buf stat.Unix.st_ino ;
  add_u32 buf mode ;
  add_u32 buf stat.Unix.st_uid ;
  add_u32 buf stat.Unix.st_gid ;
  add_u32 buf stat.Unix.st_size ;
  Buffer.add_string buf (uid :> string) ;
  add_u16 buf (Int.min (String.length (Fpath.to_string path)) 0xfff) ;
  Buffer.add_string buf (Fpath.to_string path) ;
  let len = Buffer.length buf - start in
  let padding = 8 - (len land 7) in
  Buffer.add_string buf (String.make padding '\000')

let write ~git_dir ~worktree entries =
  let fn { path = a; _ } { path = b; _ } =
    String.compare (Fpath.to_string a) (Fpath.to_string b) in
  let entries = List.sort fn entries in
  let buf = Buffer.create 0x7ff in
  Buffer.add_string buf "DIRC" ;
  add_u32 buf 2 ;
  add_u32 buf (List.length entries) ;
  List.iter (add_entry buf ~worktree) entries ;
  let contents = Buffer.contents buf in
  let checksum = Digestif.SHA1.(to_raw_string (digest_string contents)) in
  let oc = open_out_bin Fpath.(to_string (git_dir / "index")) in
  Fun.protect ~finally:(fun () -> close_out oc) @@ fun () ->
  output_string oc contents ;
  output_string oc checksum

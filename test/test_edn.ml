open Mfetch

let edn = Alcotest.testable Edn.pp Edn.equal

let edn00 =
  Alcotest.test_case "edn00" `Quick @@ fun () ->
  Alcotest.(check edn)
    "x509"
    (Edn.Opam { name = "x509"; version = None })
    (Edn.of_string_exn "x509") ;
  Alcotest.(check edn)
    "randomconv.0.2.0"
    (Edn.Opam { name = "randomconv"; version = Some "0.2.0" })
    (Edn.of_string_exn "randomconv.0.2.0") ;
  let uri =
    "https://github.com/mirage/decompress/release/download/v1.5.3/decompress-1.5.3.tbz"
  in
  Alcotest.(check edn)
    uri
    (Edn.Archive { uri; archive = Tar_bz2 })
    (Edn.of_string_exn uri) ;
  let uri = "https://perdu.org/foo-1.0.0.tar.gz" in
  Alcotest.(check edn)
    uri
    (Edn.Archive { uri; archive = Tar_gz })
    (Edn.of_string_exn uri) ;
  let uri = "git+https://github.com/robur-coop/httpcats.git" in
  Alcotest.(check edn)
    uri
    (Edn.Git_http
       { uri = "https://github.com/robur-coop/httpcats.git"; branch = None })
    (Edn.of_string_exn uri) ;
  let uri = "git+https://github.com/robur-coop/bcfg.git#unic" in
  Alcotest.(check edn)
    uri
    (Edn.Git_http
       { uri = "https://github.com/robur-coop/bcfg.git"; branch = Some "unic" })
    (Edn.of_string_exn uri) ;
  let str = "git@github.com:dinosaure/blaze.git" in
  Alcotest.(check edn)
    str
    (Edn.Git_ssh
       {
         user = "git";
         host = "github.com";
         port = None;
         path = "dinosaure/blaze.git";
         branch = None;
       })
    (Edn.of_string_exn str) ;
  let str = "git@github.com:robur-coop/unic.git#v0.1.0" in
  Alcotest.(check edn)
    str
    (Edn.Git_ssh
       {
         user = "git";
         host = "github.com";
         port = None;
         path = "robur-coop/unic.git";
         branch = Some "v0.1.0";
       })
    (Edn.of_string_exn str) ;
  let uri = "git+ssh://git@git.robur.coop:2222/foo/bar.git" in
  Alcotest.(check edn)
    uri
    (Edn.Git_ssh
       {
         user = "git";
         host = "git.robur.coop";
         port = Some 2222;
         path = "foo/bar.git";
         branch = None;
       })
    (Edn.of_string_exn uri) ;
  let uri = "file:///home/foo/dev/bcfg" in
  Alcotest.(check edn)
    uri
    (Edn.Local { dirpath = Fpath.v "/home/foo/dev/bcfg/" })
    (Edn.of_string_exn uri) ;
  let uri = "git+file:///home/foo/dev/blaze#main" in
  Alcotest.(check edn)
    uri
    (Edn.Git_local
       { dirpath = Fpath.v "/home/foo/dev/blaze/"; branch = Some "main" })
    (Edn.of_string_exn uri)

let tests = [ edn00 ]

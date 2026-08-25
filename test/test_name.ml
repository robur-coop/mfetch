open Mfetch

let name00 =
  Alcotest.test_case "name00" `Quick @@ fun () ->
  let check str expected =
    let edn = Edn.of_string_exn str in
    let name = Edn.name edn in
    Alcotest.(check string) str expected name in
  check "x509" "x509" ;
  check "randomconv.0.2.0" "randomconv" ;
  check
    "https://github.com/mirage/decompress/releases/download/v1.5.3/decompress-1.5.3.tbz"
    "decompress-1.5.3" ;
  check "git+https://github.com/robur-coop/httpcats.git" "httpcats" ;
  check "git@github.com:dinosaure/blaze.git" "blaze" ;
  check "git+file:///home/foo/flux#unzip" "flux" ;
  check "file:///home/foo/bcfg" "bcfg"

let tests = [ name00 ]

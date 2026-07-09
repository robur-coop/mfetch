let tests = [ ("Endpoint", Test_edn.tests) ]

let () =
  Printexc.record_backtrace true ;
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  Logs.set_level ~all:true (Some Logs.Debug) ;
  Alcotest.run "mfetch" tests

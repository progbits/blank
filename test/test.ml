open OUnit2
open Parse

let input = " \t\n "

let test_scan _ =
  (* Check correct tokens are scanned. *)
  assert_equal [Space; Tab; LineFeed; Space] (scan input)

let suite = "ScannerTests" >::: ["test_scan" >:: test_scan]

let () = run_test_tt_main suite

open OUnit2
open Parse

let rec stream_to_list stream =
  match Stream.next stream with
  | l -> l :: stream_to_list stream
  | exception Stream.Failure -> []

let test_scan _ =
  (* Check correct tokens are scanned. *)
  let input = " \t\n " in
  let expected = [Space; Tab; LineFeed; Space] in
  assert_equal expected (stream_to_list (scan input))

let suite = "ScannerTests" >::: ["test_scan" >:: test_scan]

let () = run_test_tt_main suite

open OUnit2
open E2

let tests =
  "Test suite  for product"
  >::: [ ("empty" >:: fun _ -> assert_equal 1 (product []))
       ; ("singleton" >:: fun _ -> assert_equal 3 (product [ 3 ]))
       ; ("two_elements" >:: fun _ -> assert_equal 6 (product [ 3; 2 ]))
       ]
;;

let _ = run_test_tt_main tests

open OUnit2
open E2

let product_tests =
  "Test suite  for product"
  >::: [ ("empty" >:: fun _ -> assert_equal 1 (product []))
       ; ("singleton" >:: fun _ -> assert_equal 3 (product [ 3 ]))
       ; ("two_elements" >:: fun _ -> assert_equal 6 (product [ 3; 2 ]))
       ]
;;

let fifth_elemeent_tests =
  "Test suite for fifth element"
  >::: [ ("empty" >:: fun _ -> assert_equal 0 (fifth_elem []))
       ; ("list with less than 5 elem"
          >:: fun _ -> assert_equal 0 (fifth_elem [ 1; 2; 3; 4 ]))
       ; ("list with 5 elements"
          >:: fun _ -> assert_equal 7 (fifth_elem [ 1; 2; 3; 4; 7 ]))
       ; ("list with 7 elements"
          >:: fun _ -> assert_equal 7 (fifth_elem [ 1; 2; 3; 4; 7; 5; 6 ]))
       ]
;;

let sort_dec_tests =
  "Test suite for sort decending"
  >::: [ ("empty" >:: fun _ -> assert_equal [] (sort_dec []))
       ; ("acending to decending"
          >:: fun _ -> assert_equal [ 4; 3; 2; 1 ] (sort_dec [ 1; 2; 3; 4 ]))
       ; ("decending to decending"
          >:: fun _ -> assert_equal [ 4; 3; 2; 1 ] (sort_dec [ 4; 3; 2; 1 ]))
       ; ("random with repeat"
          >:: fun _ -> assert_equal [ 4; 3; 3; 2; 1 ] (sort_dec [ 3; 4; 3; 1; 2 ]))
       ]
;;

let _ = run_test_tt_main product_tests
let _ = run_test_tt_main fifth_elemeent_tests
let _ = run_test_tt_main sort_dec_tests

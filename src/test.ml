open OUnit

let basic_tests = 
  "Sinsi" >:::
  [
    "add" >::
    (fun () ->
       assert_equal 1 2;
       assert_equal 2 3);

    "subtract" >::
    (fun () ->
       assert_bool "Fail!" false)
  ]

(* Test Runner *)
let _ = run_test_tt ~verbose:false basic_tests

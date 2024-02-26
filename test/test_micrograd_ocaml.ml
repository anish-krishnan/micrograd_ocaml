let%expect_test _ =
  print_endline "Hi";
  [%expect {|
    Hello, world!
  |}]

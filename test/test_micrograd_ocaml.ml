open! Core
open Micrograd_ocaml.Gradient_node

let%expect_test "test final" =
  let x = create ~label:"x" ~value:3. in
  let square = x * x in
  let () = backwards square in
  print_s [%message (square : t)];
  [%expect
    {|
      (square
       ((node
         (Mul ((node Constant) (value 3) (gradient 6) (label x))
          ((node Constant) (value 3) (gradient 6) (label x))))
        (value 9) (gradient 1) (label "(x + x)"))) |}]

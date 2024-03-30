open! Core
open Micrograd_ocaml

let%expect_test "test final" =
  let open Gradient_node in
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

let%expect_test "Test Multi_layer_perceptron" =
  let open Neural_network in
  let mlp =
    Multi_layer_perceptron.create ~num_inputs:3 ~num_neurons_per_layer:[ 1 ]
  in
  print_s [%message (mlp : Multi_layer_perceptron.t)];
  [%expect
    {|
    (mlp
     ((layers
       (((neurons
          (((weights
             (((node Constant) (value -0.098749452221508216) (gradient 0)
               (label w_0))
              ((node Constant) (value 0.13767136507210487) (gradient 0)
               (label w_1))
              ((node Constant) (value 0.92070657439837356) (gradient 0)
               (label w_2))))
            (bias
             ((node Constant) (value -0.12738887202401694) (gradient 0)
              (label b))))))))))) |}]

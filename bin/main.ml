open! Core
module Node = Micrograd_ocaml.Value.Node

let () =
  let a = Node.create ~label:"a" ~data:2.0 in
  let b = Node.create ~label:"b" ~data:3.0 in
  let c = Node.(a * b) in
  print_s [%message (c : Node.t)];
  print_s [%message (c |> Node.init_gradient |> Node.backwards : Node.t)]
(* print_s
   [%message
     (a : Node.t)
       (b : Node.t)
       ~sum:(Node.(a + b) : Node.t)
       ~sum:(Node.(a + a) : Node.t)] *)

open! Core
module Value = Micrograd_ocaml.Value.Value

let () =
  let a = Value.create ~label:"a" ~data:1.0 in
  let b = Value.create ~label:"b" ~data:2.0 in
  print_s [%message (a : Value.t) (b : Value.t) ~sum:(Value.(a + b) : Value.t)]

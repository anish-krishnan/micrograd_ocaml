open! Core
open Micrograd_ocaml.Gradient_node

let () =
  let a = create ~label:"a" ~value:2. in
  let b = create ~label:"b" ~value:3. in
  let c = a * b in
  let () = backwards c in
  print_s [%message (c : t)]

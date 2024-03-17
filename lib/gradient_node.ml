open! Core

type node = Constant | Add of t * t | Mul of t * t | Tanh of t
[@@deriving sexp_of]

and t = { node : node; value : float; mutable gradient : float; label : string }
[@@deriving sexp_of]

let create ~label ~value = { node = Constant; value; gradient = 0.; label }

let ( + ) left right =
  {
    node = Add (left, right);
    value = left.value +. right.value;
    gradient = 0.;
    label = Printf.sprintf "(%s + %s)" left.label right.label;
  }

let ( * ) left right =
  {
    node = Mul (left, right);
    value = left.value *. right.value;
    gradient = 0.;
    label = Printf.sprintf "(%s + %s)" left.label right.label;
  }

let tanh t =
  {
    node = Tanh t;
    value =
      (Float.exp (2. *. t.value) -. 1.) /. (Float.exp (2. *. t.value) +. 1.);
    gradient = 0.;
    label = Printf.sprintf "(tanh %s)" t.label;
  }

let backwards t =
  t.gradient <- 1.;
  let rec helper t =
    match t.node with
    | Constant -> ()
    | Add (left, right) ->
        left.gradient <- left.gradient +. t.gradient;
        right.gradient <- right.gradient +. t.gradient;
        helper left;
        helper right;
        ()
    | Mul (left, right) ->
        left.gradient <- left.gradient +. (right.value *. t.gradient);
        right.gradient <- right.gradient +. (left.value *. t.gradient);
        helper left;
        helper right;
        ()
    | Tanh x ->
        x.gradient <- x.gradient +. ((1. -. Float.square x.value) *. t.gradient);
        helper x;
        ()
  in
  helper t

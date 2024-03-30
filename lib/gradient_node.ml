open! Core

type node =
  | Constant
  | Add of t * t
  | Mul of t * t
  | Pow of t * float
  | Tanh of t
  | Exp of t
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

let ( - ) left right =
  {
    node = Add (left, { right with value = -1. *. right.value });
    value = left.value -. right.value;
    gradient = 0.;
    label = Printf.sprintf "(%s - %s)" left.label right.label;
  }

let ( * ) left right =
  {
    node = Mul (left, right);
    value = left.value *. right.value;
    gradient = 0.;
    label = Printf.sprintf "(%s + %s)" left.label right.label;
  }

let ( ** ) left right =
  {
    node = Pow (left, right);
    value = left.value ** right;
    gradient = 0.;
    label = Printf.sprintf "(%s ^ %f)" left.label right;
  }

let ( / ) left right =
  {
    node = Mul (left, right ** -1.);
    value = left.value /. right.value;
    gradient = 0.;
    label = Printf.sprintf "(%s / %s)" left.label right.label;
  }

let tanh t =
  {
    node = Tanh t;
    value =
      (Float.exp (2. *. t.value) -. 1.) /. (Float.exp (2. *. t.value) +. 1.);
    gradient = 0.;
    label = Printf.sprintf "(tanh %s)" t.label;
  }

let exp t =
  {
    node = Exp t;
    value = Float.exp t.value;
    gradient = 0.;
    label = Printf.sprintf "(exp %s)" t.label;
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
        helper right
    | Mul (left, right) ->
        left.gradient <- left.gradient +. (right.value *. t.gradient);
        right.gradient <- right.gradient +. (left.value *. t.gradient);
        helper left;
        helper right
    | Pow (x, k) ->
        x.gradient <-
          (x.gradient +. Float.(x.gradient * (k * (x.value ** (k - 1.)))));
        helper x
    | Tanh x ->
        x.gradient <- x.gradient +. ((1. -. Float.square x.value) *. t.gradient);
        helper x
    | Exp x ->
        x.gradient <- x.gradient +. (x.gradient *. x.value);
        helper x
  in
  helper t

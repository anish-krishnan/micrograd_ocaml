open Core

module Data = struct
  type t = { data : float; label : string option } [@@deriving sexp_of]

  let create ~label ~data = { data; label = Some label }
  let of_float data = { data; label = None }
end

module rec Operation : sig
  type t =
    | Leaf
    | Add of { left : Node.t; right : Node.t }
    | Mul of { left : Node.t; right : Node.t }
    | Tanh of { node : Node.t }
  [@@deriving sexp_of]
end = struct
  type t =
    | Leaf
    | Add of { left : Node.t; right : Node.t }
    | Mul of { left : Node.t; right : Node.t }
    | Tanh of { node : Node.t }
  [@@deriving sexp_of]
end

and Node : sig
  type t = { data : Data.t; operation : Operation.t; gradient : float }
  [@@deriving sexp_of]

  val create : label:string -> data:float -> t
  val init_gradient : t -> t
  val backwards : t -> t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val tanh : t -> t
end = struct
  type t = { data : Data.t; operation : Operation.t; gradient : float }
  [@@deriving sexp_of]

  let create ~label ~data =
    { data = Data.create ~label ~data; operation = Leaf; gradient = 0.0 }

  let init_gradient node = { node with gradient = 1.0 }

  let rec backwards ({ operation; gradient; _ } as node) =
    match operation with
    | Leaf -> node
    | Add { left; right } ->
        {
          node with
          operation =
            Add
              {
                left = backwards { left with gradient };
                right = backwards { right with gradient };
              };
          gradient;
        }
    | Mul { left; right } ->
        {
          node with
          operation =
            Mul
              {
                left =
                  backwards { left with gradient = gradient *. right.data.data };
                right =
                  backwards { right with gradient = gradient *. left.data.data };
              };
          gradient;
        }
    | Tanh { node } ->
        { node with operation = Tanh { node = backwards node }; gradient }

  let ( + ) ({ data = left_data; _ } as left)
      ({ data = right_data; _ } as right) =
    {
      data = Data.of_float (left_data.data +. right_data.data);
      operation = Add { left; right };
      gradient = 0.0;
    }

  let ( * ) ({ data = left_data; _ } as left)
      ({ data = right_data; _ } as right) =
    {
      data = Data.of_float (left_data.data *. right_data.data);
      operation = Mul { left; right };
      gradient = 0.0;
    }

  let tanh ({ data; _ } as node) =
    {
      data = Float.tanh data.data |> Data.of_float;
      operation = Tanh { node };
      gradient = 0.0;
    }
end

open! Core

(* module Op : sig
     type t = Add | Mul [@sexp_of]
   end *)
(*
   module Value : sig
     type t [@@deriving sexp_of]

     val create : label:string -> data:float -> t
     val of_float : float -> t
     val set_label : t -> label:string -> t
     val ( + ) : t -> t -> t
     val ( * ) : t -> t -> t
   end *)
module Data : sig
  type t
end

module rec Operation : sig
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
end

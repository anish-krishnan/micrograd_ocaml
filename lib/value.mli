open! Core

module Op : sig
  type t = Add | Mul [@sexp_of]
end

module Value : sig
  type t [@@deriving sexp_of]

  val create : label:string -> data:float -> t
  val of_float : float -> t
  val set_label : t -> label:string -> t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
end

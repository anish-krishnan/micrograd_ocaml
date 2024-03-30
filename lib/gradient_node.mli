open! Core

type t [@@deriving sexp_of]

val create : label:string -> value:float -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( ** ) : t -> float -> t
val ( / ) : t -> t -> t
val tanh : t -> t
val exp : t -> t
val backwards : t -> unit

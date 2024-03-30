open! Core

module Multi_layer_perceptron : sig
  type t [@@deriving sexp_of]

  val create : num_inputs:int -> num_neurons_per_layer:int list -> t
end

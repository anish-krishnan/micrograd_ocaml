open Core

module Neuron = struct
  type t = { mutable weights : Gradient_node.t list; bias : Gradient_node.t }
  [@@deriving sexp_of]

  let create ~num_inputs =
    {
      weights =
        List.init num_inputs ~f:(fun i ->
            let label = Printf.sprintf "w_%s" (Int.to_string i) in
            Gradient_node.create ~label ~value:(Random.float_range (-1.) 1.));
      bias =
        Gradient_node.create ~label:"b" ~value:(Random.float_range (-1.) 1.);
    }

  let forward { weights; bias } (xs : Gradient_node.t list) =
    let activation =
      Gradient_node.(
        (List.zip_exn weights xs
        |> List.map ~f:(fun (weight, x) -> weight * x)
        |> List.reduce_exn ~f:Gradient_node.( + ))
        + bias)
    in
    Gradient_node.tanh activation
end

module Layer = struct
  type t = { neurons : Neuron.t list } [@@deriving sexp_of]

  let create ~num_inputs_per_neuron ~num_neurons =
    {
      neurons =
        List.init num_neurons ~f:(fun _ ->
            Neuron.create ~num_inputs:num_inputs_per_neuron);
    }

  let forward { neurons } xs =
    List.map neurons ~f:(fun neuron -> Neuron.forward neuron xs)
end

module Multi_layer_perceptron = struct
  type t = { layers : Layer.t list } [@@deriving sexp_of]

  let create ~num_inputs ~num_neurons_per_layer =
    let num_neurons_per_layer_including_input =
      num_inputs :: num_neurons_per_layer
    in
    let zipped, _ =
      List.zip_with_remainder num_neurons_per_layer_including_input
        num_neurons_per_layer
    in
    {
      layers =
        List.map zipped
          ~f:(fun (num_neurons_in_prev_layer, num_neurons_in_this_layer) ->
            Layer.create ~num_inputs_per_neuron:num_neurons_in_prev_layer
              ~num_neurons:num_neurons_in_this_layer);
    }

  let _forward { layers } xs =
    List.fold layers ~init:xs ~f:(fun accum layer -> Layer.forward layer accum)
end

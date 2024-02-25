open Core

module Op = struct
  type t = Add | Mul [@@deriving sexp_of]
end

module Value = struct
  type t = {
    data : float;
    label : string option;
    op : Op.t option;
    children : t list;
  }
  [@@deriving sexp_of]

  let create ~label ~data =
    { data; label = Some label; children = []; op = None }

  let create_with_op ~data ~op ~children =
    { data; label = None; children; op = Some op }

  let of_float data = { data; label = None; children = []; op = None }
  let set_label t ~label = { t with label = Some label }

  let sexp_of_t { data; label; _ } =
    [%message "Value" (label : string option) (data : float)]

  let ( + ) ({ data = left_data; _ } as left)
      ({ data = right_data; _ } as right) =
    create_with_op ~data:(left_data +. right_data) ~op:Op.Add
      ~children:[ left; right ]

  let ( * ) ({ data = left_data; _ } as left)
      ({ data = right_data; _ } as right) =
    create_with_op ~data:(left_data *. right_data) ~op:Op.Mul
      ~children:[ left; right ]
end

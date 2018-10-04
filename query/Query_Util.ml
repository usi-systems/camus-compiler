open Core

let format_list ?sep:(s=",") formatter l =
  String.concat
    ~sep:s
    (List.map
      l
      ~f:formatter)

let int_exp x y = (float_of_int x) ** (float_of_int y) |> int_of_float

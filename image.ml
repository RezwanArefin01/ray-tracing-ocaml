open Base

type t =
  { dim: Dim2d.t
  ; pixels : Color.t array
  }

let create ?(default = { Color.r = 0.0; g = 0.0; b = 0.0 }) dim =
  { dim; pixels = Array.create ~len:(dim.height * dim.width) default }
;;

let set_pixel t row col color = t.pixels.((row * t.dim.width) + col) <- color

let to_string t =
  String.concat
    ~sep:"\n"
    [ "P3"
    ; Printf.sprintf "%d %d" t.dim.height t.dim.width
    ; "256"
    ; String.concat_array ~sep:"\n" (Array.map t.pixels ~f:Color.to_string)
    ]
;;

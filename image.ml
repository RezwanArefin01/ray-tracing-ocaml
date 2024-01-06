open Base

type t =
  { height : int
  ; width : int
  ; pixels : Color.t array
  }

let create ?(default = { Color.r = 0.0; g = 0.0; b = 0.0 }) height width =
  { height; width; pixels = Array.create ~len:(height * width) default }
;;

let height t = t.height
let width t = t.width
let set_pixel t row col color = t.pixels.((row * t.width) + col) <- color

let to_string t =
  String.concat
    ~sep:"\n"
    [ "P3"
    ; Printf.sprintf "%d %d" t.height t.width
    ; "256"
    ; String.concat_array ~sep:"\n" (Array.map t.pixels ~f:Color.to_string)
    ]
;;

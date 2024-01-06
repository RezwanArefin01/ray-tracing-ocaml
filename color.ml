open Base

type t =
  { r : float
  ; g : float
  ; b : float
  }

let quantize v =
  let v = Float.clamp_exn v ~min:0.0 ~max:1.0 in
  Int.of_float (v *. 255.0)
;;

let to_string t = Printf.sprintf "%d %d %d" (quantize t.r) (quantize t.g) (quantize t.b)

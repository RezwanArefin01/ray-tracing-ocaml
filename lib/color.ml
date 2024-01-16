open Base
include Vec3

let quantize v =
  let v = Float.clamp_exn v ~min:0.0 ~max:1.0 in
  Int.of_float Float.(v * 255.0)
;;

let to_string t = Printf.sprintf "%d %d %d" (quantize t.x) (quantize t.y) (quantize t.z)
let to_graphics_color t = Graphics.rgb (quantize t.x) (quantize t.y) (quantize t.z)
let gamma_correct t = { x = Float.sqrt t.x; y = Float.sqrt t.y; z = Float.sqrt t.z }
let black = { x = 0.; y = 0.; z = 0. }
let white = { x = 1.; y = 1.; z = 1. }
let red = { x = 1.; y = 0.; z = 0. }
let green = { x = 0.; y = 1.; z = 0. }
let blue = { x = 0.; y = 0.; z = 1. }
let skyblue = { x = 0.5; y = 0.7; z = 1. }

open Base
include Vec3

let to_string t =
  let gamma_correct v = Float.sqrt v in
  let quantize v =
    let v = Float.clamp_exn (gamma_correct v) ~min:0.0 ~max:1.0 in
    Int.of_float Float.(v * 255.0)
  in
  Printf.sprintf "%d %d %d" (quantize t.x) (quantize t.y) (quantize t.z)
;;

open Base

type t =
  { x : float
  ; y : float
  ; z : float
  }
[@@deriving fields, sexp]

let ( ~- ) t = Float.{ x = -t.x; y = -t.y; z = -t.z }
let ( + ) p q = Float.{ x = p.x + q.x; y = p.y + q.y; z = p.z + q.z }
let ( - ) p q = Float.{ x = p.x - q.x; y = p.y - q.y; z = p.z - q.z }
let ( * ) p q = Float.{ x = p.x * q.x; y = p.y * q.y; z = p.z * q.z }
let ( *. ) c t = Float.{ x = c * t.x; y = c * t.y; z = c * t.z }
let ( /. ) t c = Float.{ x = t.x / c; y = t.y / c; z = t.z / c }
let ( +. ) t c = Float.{ x = t.x + c; y = t.y + c; z = t.z + c }
let ( -. ) t c = Float.{ x = t.x - c; y = t.y - c; z = t.z - c }
let dot p q = Float.((p.x * q.x) + (p.y * q.y) + (p.z * q.z))

let cross p q =
  Float.
    { x = (p.y * q.z) - (p.z * q.y)
    ; y = (p.z * q.x) - (p.x * q.z)
    ; z = (p.x * q.y) - (p.y * q.x)
    }
;;

let length2 t = Float.((t.x * t.x) + (t.y * t.y) + (t.z * t.z))
let length t = Float.sqrt (length2 t)
let to_string t = Printf.sprintf "(%.3f, %.3f, %.3f)" t.x t.y t.z
let unit_vec t = t /. length t
let unit_x = { x = 1.0; y = 0.0; z = 0.0 }
let unit_y = { x = 0.0; y = 1.0; z = 0.0 }
let unit_z = { x = 0.0; y = 0.0; z = 1.0 }

let rec random_in_unit () =
  let open Float.O in
  let random () = Random.float_range ~-.1.0 1.0 in 
  let v = { x = random (); y = random () ; z = random ()} in
  if length2 v < 1.0 then v else random_in_unit ()
;;

let random_on_unit () = unit_vec (random_in_unit ())

let rec random_on_hemisphere normal =
  let v = random_on_unit () in
  if Float.(dot v normal > 0.0) then v else -v
;;

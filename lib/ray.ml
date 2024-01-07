type t =
  { orig : Point.t
  ; dir : Vec3.t
  }
[@@deriving fields, sexp]

let at t c = Vec3.(t.orig + (c *. t.dir))

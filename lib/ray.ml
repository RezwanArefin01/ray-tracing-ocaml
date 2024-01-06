type t =
  { orig : Point.t
  ; dir : Vec3.t
  }

let at t c = Vec3.(t.orig + (c *. t.dir))

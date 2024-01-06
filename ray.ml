open Base

type t =
  { ori : Point.t
  ; dir : Vec3.t
  }

let at t c = Vec3.(t.ori + (c *. t.dir))

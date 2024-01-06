open Base

type t =
  { orig : Vec3.t
  ; dir : Vec3.t
  }

val at : t -> float -> Vec3.t

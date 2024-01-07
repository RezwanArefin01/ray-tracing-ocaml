open Base

type t =
  { orig : Vec3.t
  ; dir : Vec3.t
  }
[@@deriving fields, sexp]

val at : t -> float -> Vec3.t

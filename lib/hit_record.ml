open Base

type t =
  { p : Point.t
  ; normal : Vec3.t
  ; t : float
  }
[@@deriving fields, sexp]
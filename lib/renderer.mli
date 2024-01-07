open Base

type t =
  { height : int
  ; aspect_ratio : float
  ; viewport_height : float
  ; focal_length : float
  ; camera_center : Vec3.t
  }

val render : t -> (module Shapes.Shape_instance) list -> Image.t

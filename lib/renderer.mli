open Base

type t =
  { height : int
  ; aspect_ratio : float
  ; vfov: float
  ; samples_per_pixel : int
  ; max_depth : int
  }

val render : t -> (module Shapes.Shape_instance) list -> Image.t

open Base

type t =
  { height : int
  ; aspect_ratio : float
  ; vfov: float
  ; samples_per_pixel : int
  }

val render : t -> (module Shapes.Shape_instance) list -> Image.t

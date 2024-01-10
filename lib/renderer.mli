open Base

type t =
  { height : int
  ; aspect_ratio : float
  ; vfov: float
  ; max_depth : int
  }

val render : t -> (module Shapes.Shape_instance) list -> unit

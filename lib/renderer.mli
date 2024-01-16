open Base

type t

val create : height:int -> aspect_ratio:float -> vfov:float -> max_depth:int -> t

val render : t -> (module Shapes.Shape_instance) list -> unit

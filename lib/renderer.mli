open Base
open Domainslib

type t

val height : t -> int
val width : t -> int
val image : t -> Image.t
val samples_per_pixel : t -> int

val create
  :  height:int
  -> aspect_ratio:float
  -> vfov:float
  -> max_depth:int
  -> shapes:(module Shapes.Shape_instance) list
  -> t

val step : t -> Task.pool -> unit

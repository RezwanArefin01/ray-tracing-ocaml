open Base

type t

val width : t -> int
val height : t -> int
val create : ?default:Color.t -> int -> int -> t
val set_pixel : t -> int -> int -> Color.t -> unit
val to_string : t -> string

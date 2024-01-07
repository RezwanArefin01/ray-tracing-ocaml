open Base

type t =
  { height : int
  ; width : int
  ; pixels : Color.t array
  }
[@@deriving fields, sexp]

val create : ?default:Color.t -> int -> int -> t
val set_pixel : t -> int -> int -> Color.t -> unit
val to_string : t -> string

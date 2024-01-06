open Base

type t =
  { x : float
  ; y : float
  ; z : float
  }
[@@deriving fields]

val ( ~- ) : t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( *. ) : float -> t -> t
val ( /. ) : t -> float -> t
val dot : t -> t -> float
val cross : t -> t -> t
val length2 : t -> float
val length : t -> float
val to_string : t -> string
val unit_vec : t -> t
val unit_x : t
val unit_y : t
val unit_z : t

open Base

type t =
  { p : Point.t
  ; normal : Vec3.t
  ; t : float
  }
[@@deriving fields, sexp]

let closest hit_records =
  List.fold hit_records ~init:None ~f:(fun closest record ->
    match closest, record with
    | (Some x as x'), (Some y as y') -> if Float.(x.t < y.t) then x' else y'
    | (Some x as x'), _ -> x'
    | _, (Some y as y') -> y'
    | _, _ -> None)
;;

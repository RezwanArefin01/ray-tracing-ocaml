open Base

module type Shape = sig
  type t [@@deriving sexp]

  val hit : t -> Ray.t -> tmin:float -> tmax:float -> Hit_record.t option
end

module type Shape_instance = sig
  module Shape : Shape

  val this : Shape.t
end

let create (type a) (module Shape : Shape with type t = a) this : (module Shape_instance) =
  (module struct
    module Shape = Shape

    let this = this
  end)
;;

let hit (module Instance : Shape_instance) ray ~tmin ~tmax =
  Instance.Shape.hit Instance.this ray ~tmin ~tmax
;;

module Sphere = struct
  type t =
    { center : Point.t
    ; radius : float
    }
  [@@deriving sexp]

  let hit s (ray : Ray.t) ~tmin ~tmax : Hit_record.t option =
    let open Float.O in
    let oc = Vec3.(ray.orig - s.center) in
    let a = Vec3.length2 ray.dir in
    let b_half = Vec3.dot ray.dir oc in
    let c = Vec3.length2 oc - (s.radius * s.radius) in
    let d = (b_half * b_half) - (a * c) in
    let t =
      if d < 0.0
      then None
      else (
        let sqrtD = Float.sqrt d in
        let t1 = (-b_half - sqrtD) / a in
        let t2 = (-b_half + sqrtD) / a in
        (* TODO: return root resulting in closer point instead of the minimum one *)
        match tmin <= t1 && t1 <= tmax, tmin <= t2 && t2 <= tmax with
        | true, true -> Some (Float.min t1 t2)
        | true, _ -> Some t1
        | _, true -> Some t2
        | _, _ -> None)
    in
    match t with
    | None -> None
    | Some t ->
      let p = Ray.at ray t in
      let normal = Vec3.((p - s.center) /. s.radius) in
      Some { p; normal; t }
  ;;
end

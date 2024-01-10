open Base

type t =
  { height : int
  ; aspect_ratio : float
  ; vfov: float 
  }

type side =
  | Inside
  | Outside

let side (ray : Ray.t) outside_normal =
  let open Float.O in
  if Vec3.dot ray.dir outside_normal > 0. then Inside else Outside
;;

let ray_color (ray : Ray.t) (shapes : (module Shapes.Shape_instance) list) : Color.t =
  let unit = Vec3.unit_vec ray.dir in
  let a = 0.5 *. (unit.y +. 1.0) in
  let default =
    Vec3.(
      (Float.(1.0 - a) *. { x = 1.; y = 1.; z = 1. }) + (a *. { x = 0.5; y = 0.7; z = 1. }))
  in
  let hit_record_option =
    shapes
    |> List.map ~f:(fun shape -> Shapes.hit shape ray ~tmin:Float.zero ~tmax:Float.infinity)
    |> List.fold
         ~init:(None : Hit_record.t option)
         ~f:(fun acc hit_record ->
           match acc, hit_record with
           | Some x, Some y -> if Float.(x.t < y.t) then acc else hit_record
           | Some x, _ -> acc
           | _, Some y -> hit_record
           | _, _ -> None)
  in
  match hit_record_option with
  | None -> default
  | Some h -> Vec3.(0.5 *. (h.normal +. 1.))
;;

let render t (shapes : (module Shapes.Shape_instance) list) =
  let width = Int.of_float (Float.of_int t.height *. t.aspect_ratio) in
  let viewport_height = 2.0 *. Float.tan (t.vfov *. Float.pi /. 360.) in
  let viewport_width = viewport_height *. t.aspect_ratio in
  let viewport_x = Vec3.{ x = 0.0; y = Float.(-viewport_height); z = 0.0 } in
  let viewport_y = Vec3.{ x = viewport_width; y = 0.0; z = 0.0 } in
  let pixel_delta_x = Vec3.(viewport_x /. Float.of_int t.height) in
  let pixel_delta_y = Vec3.(viewport_y /. Float.of_int width) in
  let camera_center = Point.{x = 0.; y = 0.; z = 0.} in
  let pixel00 =
    Point.(
      camera_center
      - Vec3.unit_z
      - (viewport_x /. 2.0)
      - (viewport_y /. 2.0)
      + ((pixel_delta_x + pixel_delta_y) /. 2.0))
  in
  let image = Image.create t.height width in
  for i = 0 to image.height - 1 do
    for j = 0 to image.width - 1 do
      let pixel_center =
        Vec3.(
          pixel00 + (Float.of_int i *. pixel_delta_x) + (Float.of_int j *. pixel_delta_y))
      in
      let ray : Ray.t =
        { orig = camera_center; dir = Vec3.(pixel_center - camera_center) }
      in
      let color = ray_color ray shapes in
      Image.set_pixel image i j color
    done
  done;
  image
;;

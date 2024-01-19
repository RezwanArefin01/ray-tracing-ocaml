open Base
open Domainslib

type t =
  { height : int
  ; width : int
  ; max_depth : int
  ; pixel00 : Point.t
  ; pixel_delta_x : Vec3.t
  ; pixel_delta_y : Vec3.t
  ; camera_center : Point.t
  ; image : Image.t
  ; mutable samples_per_pixel : int
  ; shapes : (module Shapes.Shape_instance) list
  }
[@@deriving fields]

let create ~height ~aspect_ratio ~vfov ~max_depth ~shapes =
  let width = Int.of_float (Float.of_int height *. aspect_ratio) in
  let viewport_height = 2.0 *. Float.tan (vfov *. Float.pi /. 360.) in
  let viewport_width = viewport_height *. aspect_ratio in
  let viewport_x = Vec3.{ x = 0.0; y = Float.(-viewport_height); z = 0.0 } in
  let viewport_y = Vec3.{ x = viewport_width; y = 0.0; z = 0.0 } in
  let pixel_delta_x = Vec3.(viewport_x /. Float.of_int height) in
  let pixel_delta_y = Vec3.(viewport_y /. Float.of_int width) in
  let camera_center = Point.{ x = 0.; y = 0.; z = 0. } in
  let pixel00 =
    Point.(
      camera_center
      - Vec3.unit_z
      - (viewport_x /. 2.0)
      - (viewport_y /. 2.0)
      + ((pixel_delta_x + pixel_delta_y) /. 2.0))
  in
  let image = Image.create height width in
  { height
  ; width
  ; max_depth
  ; pixel00
  ; pixel_delta_x
  ; pixel_delta_y
  ; camera_center
  ; image
  ; samples_per_pixel = 0
  ; shapes
  }
;;

type side =
  | Inside
  | Outside

let side (ray : Ray.t) outside_normal =
  let open Float.O in
  if Vec3.dot ray.dir outside_normal > 0. then Inside else Outside
;;

let rec ray_color (ray : Ray.t) (shapes : (module Shapes.Shape_instance) list) depth =
  if depth <= 0
  then Color.black
  else (
    let closest_hit =
      shapes
      |> List.map ~f:(fun shape -> Shapes.hit shape ray ~tmin:0.001 ~tmax:Float.infinity)
      |> Hit_record.closest
    in
    match closest_hit with
    | None ->
      let unit = Vec3.unit_vec ray.dir in
      let a = 0.5 *. (unit.y +. 1.0) in
      Vec3.((Float.(1.0 - a) *. Color.white) + (a *. Color.skyblue))
    | Some h ->
      let child_ray = Ray.{ orig = h.p; dir = Vec3.(h.normal + random_in_unit ()) } in
      Vec3.(0.5 *. ray_color child_ray shapes Int.(depth - 1)))
;;

let step t pool =
  let c = Float.of_int t.samples_per_pixel in
  Task.parallel_for pool ~chunk_size:50 ~start:0 ~finish:(t.height - 1) ~body:(fun i ->
    for j = 0 to t.width - 1 do
      let random_x = Float.of_int i +. Random.float_range ~-.1.0 1.0 in
      let random_y = Float.of_int j +. Random.float_range ~-.1.0 1.0 in
      let sample_point =
        Vec3.(t.pixel00 + (random_x *. t.pixel_delta_x) + (random_y *. t.pixel_delta_y))
      in
      let ray : Ray.t =
        { orig = t.camera_center; dir = Vec3.(sample_point - t.camera_center) }
      in
      let color = ray_color ray t.shapes t.max_depth in
      t.image.(i).(j) <- Vec3.(((c *. t.image.(i).(j)) + color) /. Float.(c + 1.))
    done);
  t.samples_per_pixel <- t.samples_per_pixel + 1
;;

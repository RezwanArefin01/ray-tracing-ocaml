open Base
open Stdio

type t =
  { height : int
  ; aspect_ratio : float
  ; vfov : float
  ; samples_per_pixel : int
  ; max_depth : int
  }

type side =
  | Inside
  | Outside

let side (ray : Ray.t) outside_normal =
  let open Float.O in
  if Vec3.dot ray.dir outside_normal > 0. then Inside else Outside
;;

let rec ray_color (ray : Ray.t) (shapes : (module Shapes.Shape_instance) list) depth
  =
  if depth <= 0
  then Color.{ x = 0.; y = 0.; z = 0. }
  else (
    let hit_record_option =
      shapes
      |> List.map ~f:(fun shape ->
        Shapes.hit shape ray ~tmin:0.001 ~tmax:Float.infinity)
      |> List.fold
           ~init:(None : Hit_record.t option)
           ~f:(fun acc hit_record ->
             match acc, hit_record with
             | Some x, Some y -> if Float.(x.t < y.t) then acc else hit_record
             | Some x, _ -> acc
             | _, Some y -> hit_record
             | _, _ -> None)
    in
    let unit = Vec3.unit_vec ray.dir in
    let a = 0.5 *. (unit.y +. 1.0) in
    let default =
      Vec3.(
        (Float.(1.0 - a) *. { x = 1.; y = 1.; z = 1. })
        + (a *. { x = 0.5; y = 0.7; z = 1. }))
    in
    match hit_record_option with
    | None -> default
    | Some h -> 
      let child_ray = Ray.{ orig = h.p; dir = Vec3.random_on_hemisphere h.normal } in
      Vec3.(1.0 *. ray_color child_ray shapes Int.(depth - 1))
  )
;;

let render t (shapes : (module Shapes.Shape_instance) list) =
  let width = Int.of_float (Float.of_int t.height *. t.aspect_ratio) in
  let viewport_height = 2.0 *. Float.tan (t.vfov *. Float.pi /. 360.) in
  let viewport_width = viewport_height *. t.aspect_ratio in
  let viewport_x = Vec3.{ x = 0.0; y = Float.(-viewport_height); z = 0.0 } in
  let viewport_y = Vec3.{ x = viewport_width; y = 0.0; z = 0.0 } in
  let pixel_delta_x = Vec3.(viewport_x /. Float.of_int t.height) in
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
  let image = Image.create t.height width in
  for i = 0 to image.height - 1 do
    eprintf "\rRendering line %d/%d" (i + 1) image.height;
    Out_channel.flush stderr;
    for j = 0 to image.width - 1 do
      let sample_colors =
        Array.init t.samples_per_pixel ~f:(fun _ ->
          let random_x = Float.of_int i +. Random.float_range ~-.1.0 1.0 in
          let random_y = Float.of_int j +. Random.float_range ~-.1.0 1.0 in
          let sample_point =
            Vec3.(pixel00 + (random_x *. pixel_delta_x) + (random_y *. pixel_delta_y))
          in
          let ray : Ray.t =
            { orig = camera_center; dir = Vec3.(sample_point - camera_center) }
          in
          ray_color ray shapes t.max_depth)
      in
      let color = Array.reduce_exn sample_colors ~f:Vec3.( + ) in
      let color = Vec3.(color /. Float.of_int t.samples_per_pixel) in
      Image.set_pixel image i j color
    done
  done;
  eprintf "\r";
  Out_channel.flush stderr;
  image
;;

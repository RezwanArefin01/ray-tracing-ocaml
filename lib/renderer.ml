open Base
open Stdio
module Time = Core.Time_float

type t =
  { height : int
  ; aspect_ratio : float
  ; vfov : float
  ; max_depth : int
  }

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
    let hit_record_option =
      shapes
      |> List.map ~f:(fun shape -> Shapes.hit shape ray ~tmin:0.001 ~tmax:Float.infinity)
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
      Vec3.((Float.(1.0 - a) *. Color.white) + (a *. { x = 0.5; y = 0.7; z = 1. }))
    in
    match hit_record_option with
    | None -> default
    | Some h ->
      let child_ray = Ray.{ orig = h.p; dir = Vec3.(h.normal + random_in_unit ()) } in
      Vec3.(0.5 *. ray_color child_ray shapes Int.(depth - 1)))
;;

let render t (shapes : (module Shapes.Shape_instance) list) =
  let height = t.height in
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
  let samples_per_pixel = ref 0 in
  Graphics.open_graph (Printf.sprintf " %dx%d" width height);
  Graphics.set_text_size 100;
  Graphics.display_mode false;
  while true do
    let starting = Time.now () in 
    for i = 0 to height - 1 do
      for j = 0 to width - 1 do
        let random_x = Float.of_int i +. Random.float_range ~-.1.0 1.0 in
        let random_y = Float.of_int j +. Random.float_range ~-.1.0 1.0 in
        let sample_point =
          Vec3.(pixel00 + (random_x *. pixel_delta_x) + (random_y *. pixel_delta_y))
        in
        let ray : Ray.t =
          { orig = camera_center; dir = Vec3.(sample_point - camera_center) }
        in
        let color = ray_color ray shapes t.max_depth in
        let c = Float.of_int !samples_per_pixel in
        image.(i).(j) <- Vec3.(((c *. image.(i).(j)) + color) /. Float.(c + 1.))
      done
    done;
    samples_per_pixel := !samples_per_pixel + 1;
    Graphics.clear_graph (); 
    Graphics.draw_image (Image.to_graphics_image image) 0 0; 
    let ending = Time.now () in
    let duration = Time.diff ending starting in
    let fps = 1. /. Time.Span.to_sec duration in
    Graphics.moveto 10 (height - 20);
    Graphics.draw_string (Printf.sprintf "FPS: %f" fps);
    Graphics.moveto 10 (height - 40);
    Graphics.draw_string (Printf.sprintf "RPP: %d" !samples_per_pixel);
    Graphics.synchronize ();
    eprintf "Frame %d: %f ms\n" !samples_per_pixel (Time.Span.to_ms (Time.diff ending starting));
    Out_channel.flush stderr;
  done
;;

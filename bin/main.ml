open Base
open Stdio
open Domainslib
open Renderer_lib
module Time = Core.Time_float

let argv = Sys.get_argv ()

let num_domains =
  try Int.of_string argv.(1) with
  | _ -> 12
;;

let pool = Task.setup_pool ~num_domains ()

let init_graphics r =
  Graphics.open_graph (Printf.sprintf " %dx%d" (Renderer.width r) (Renderer.height r));
  Graphics.set_text_size 100;
  Graphics.display_mode false
;;

let show_image r fps avg_fps =
  let height = Renderer.height r in
  let spp = Renderer.samples_per_pixel r in
  Graphics.clear_graph ();
  Graphics.draw_image (Image.to_graphics_image (Renderer.image r)) 0 0;
  Graphics.moveto 10 (height - 20);
  Graphics.draw_string (Printf.sprintf "Frame: %d" spp);
  Graphics.moveto 10 (height - 40);
  Graphics.draw_string (Printf.sprintf "FPS: %d" (Int.of_float fps));
  Graphics.moveto 10 (height - 60);
  Graphics.draw_string (Printf.sprintf "Average FPS: %d" (Int.of_float avg_fps));
  Graphics.synchronize ()
;;

let build_scene () =
  let center : Point.t = { x = 0.0; y = 0.; z = -1. } in
  let radius = 0.5 in
  let sphere1 = Shapes.create (module Shapes.Sphere) { center; radius } in
  let center : Point.t = { x = 0.0; y = -200.5; z = -1. } in
  let radius = 200. in
  let world = Shapes.create (module Shapes.Sphere) { center; radius } in
  [ sphere1; world ]
;;

let () =
  let height = 600 in
  let aspect_ratio = 16. /. 10. in
  let vfov = 90. in
  let max_depth = 10 in
  let shapes = build_scene () in
  let renderer = Renderer.create ~height ~aspect_ratio ~vfov ~max_depth ~shapes in
  try
    init_graphics renderer;
    let avg_fps = ref 0.0 in
    while true do
      let starting = Time.now () in
      Task.run pool (fun () -> Renderer.step renderer pool);
      let ending = Time.now () in
      let duration = Time.Span.to_sec (Time.diff ending starting) in
      let fps = 1. /. duration in
      avg_fps := (0.1 *. fps) +. (0.9 *. !avg_fps);
      show_image renderer fps !avg_fps
    done
  with
  | _ -> Image.print (Renderer.image renderer) stdout
;;

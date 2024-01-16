open Base
open Renderer_lib

let init_graphics r =
  Graphics.open_graph (Printf.sprintf " %dx%d" (Renderer.width r) (Renderer.height r));
  Graphics.set_text_size 100;
  Graphics.display_mode false
;;

let show_image r =
  let height = Renderer.height r in
  let spp = Renderer.samples_per_pixel r in
  Graphics.clear_graph ();
  Graphics.draw_image (Image.to_graphics_image (Renderer.image r)) 0 0;
  (* Graphics.moveto 10 (height - 20);
     Graphics.draw_string (Printf.sprintf "FPS: %f" fps); *)
  Graphics.moveto 10 (height - 40);
  Graphics.draw_string (Printf.sprintf "RPP: %d" spp);
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
  init_graphics renderer;
  while true do
    Renderer.step renderer;
    show_image renderer
  done
;;

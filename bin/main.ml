open Base
open Renderer_lib
open Stdio

let () =
  let renderer : Renderer.t =
    { height = 600
    ; aspect_ratio = 16. /. 10.
    ; viewport_height = 2.
    ; focal_length = 1.
    ; camera_center : Point.t = { x = 0.; y = 0.; z = 0. }
    }
  in
  let image = Renderer.render renderer () in
  printf "%s\n" (Image.to_string image)
;;

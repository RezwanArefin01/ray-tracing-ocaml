open Base
open Renderer_lib
open Stdio

let () =
  let center : Point.t = { x = 0.; y = 0.; z = -5.} in
  let radius = 2. in 
  let sphere1 = Shapes.create (module Shapes.Sphere) { center; radius } in

  let center : Point.t = { x = -5.; y = 0.; z = -5.} in
  let radius = 2. in 
  let sphere2 = Shapes.create (module Shapes.Sphere) { center; radius } in

  let renderer : Renderer.t =
    { height = 600
    ; aspect_ratio = 16. /. 10.
    ; viewport_height = 2.
    ; focal_length = 1.
    ; camera_center : Point.t = { x = 0.; y = 0.; z = 0. }
    }
  in
  let image = Renderer.render renderer [sphere1; sphere2] in
  printf "%s\n" (Image.to_string image)
;;

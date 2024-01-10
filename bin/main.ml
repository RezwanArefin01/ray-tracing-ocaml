open Base
open Renderer_lib
open Stdio

let () =
  let center : Point.t = { x = 1.0; y = 0.; z = -1.} in
  let radius = 0.3 in 
  let sphere1 = Shapes.create (module Shapes.Sphere) { center; radius } in

  let center : Point.t = { x = -1.0; y = 0.; z = -1.} in
  let radius = 0.3 in 
  let sphere2 = Shapes.create (module Shapes.Sphere) { center; radius } in

  let center : Point.t = { x = 0.0; y = -200.5; z = -3.} in
  let radius = 200. in 
  let world = Shapes.create (module Shapes.Sphere) { center; radius } in


  let renderer : Renderer.t =
    { height = 600
    ; aspect_ratio = 16. /. 10.
    ; vfov = 70.
    }
  in
  let image = Renderer.render renderer [sphere1; sphere2; world] in
  printf "%s\n" (Image.to_string image)
;;

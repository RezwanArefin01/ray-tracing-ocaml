open Base
open Renderer_lib
open Stdio

let () =
  let ray : Ray.t = { orig = { x = 0.; y = 0.; z = 0.}; dir = { x = -1.; y = 0.; z = 0.} } in
  let sphere = Shapes.create (module Shapes.Sphere) { center = { x = 10.; y = 0.; z = 0.}; radius = 1. } in
  let hit = Shapes.hit sphere ray ~tmin:~-.100. ~tmax:100. in 
  match hit with
  | None -> printf "Did not hit\n"
  | Some hit -> printf "%s\n" (Sexp.to_string (Hit_record.sexp_of_t hit))
;;

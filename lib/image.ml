open Base
open Stdio

type t = Color.t array array

let create height width =
  Array.init height ~f:(fun _ -> Array.create ~len:width Color.black)
;;

let set_pixel t row col color = t.(row).(col) <- color
let dimentions t = Array.length t, Array.length t.(0)

let print t channel =
  let height, width = dimentions t in
  let fprintf = Out_channel.fprintf in
  fprintf channel "P3\n";
  fprintf channel "%d %d\n" width height;
  fprintf channel "255\n";
  Array.iter t ~f:(fun row ->
    Array.iter row ~f:(fun elem ->
      fprintf channel "%s\n" (Color.to_string (Color.gamma_correct elem))))
;;

let to_graphics_image t =
  let height, width = dimentions t in
  let image = Array.init height ~f:(fun _ -> Array.create ~len:width Graphics.black) in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      image.(i).(j) <- Color.to_graphics_color (Color.gamma_correct t.(i).(j))
    done
  done;
  Graphics.make_image image
;;

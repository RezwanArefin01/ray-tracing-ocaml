open Base

type t =
  { height : int
  ; aspect_ratio : float
  ; viewport_height : float
  ; focal_length : float
  ; camera_center : Point.t
  }

let ray_color ray () = { Color.r = Random.float 1.0; g = Random.float 1.0;  b = Random.float 1. }

let render t () =
  let viewport_width = t.viewport_height *. t.aspect_ratio in
  let viewport_x : Vec3.t = { x = 0.0; y = Float.(-t.viewport_height); z = 0.0 } in
  let viewport_y : Vec3.t = { x = viewport_width; y = 0.0; z = 0.0 } in
  let pixel_delta_x = Vec3.(viewport_x /. t.viewport_height) in
  let pixel_delta_y = Vec3.(viewport_y /. viewport_width) in
  let pixel000 : Point.t =
    Vec3.(
      t.camera_center
      - (t.focal_length *. Vec3.unit_z)
      - (viewport_x /. 2.0)
      - (viewport_y /. 2.0)
      + ((pixel_delta_x + pixel_delta_y) /. 2.0))
  in
  let image =
    Image.create t.height (Int.of_float (Float.of_int t.height *. t.aspect_ratio))
  in
  for i = 0 to image.height - 1 do
    for j = 0 to image.width - 1 do
      let pixel_center =
        Vec3.(
          pixel000 + (Float.of_int i *. pixel_delta_x) + (Float.of_int j *. pixel_delta_y))
      in
      let ray : Ray.t =
        { orig = t.camera_center; dir = Vec3.(pixel_center - t.camera_center) }
      in
      let color = ray_color ray () in
      Image.set_pixel image i j color
    done
  done;
  image
;;

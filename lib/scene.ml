[@@@warning "-69-27-33"]

open Ray
open Vector

open Camera
open Light
open Shape
open Core
open Color

module Intersection_record = struct
  type t = {intersection_time: float; position: Vector3f.t; normal: Vector3f.t; }
end

module Scene = struct
  type t = {camera:Camera.t; lights:(module L) list; shapes:(module S) list}
  let get_color _ ~i ~j =  Color.make 0. 0. 0.

  let ray_trace {camera; lights; shapes} ~width ~height ~rLimit ~cLimit : Color.t list list = 
    let get_color_at i j =
      Camera.get_ray camera |> get_color ~i ~j
    in
    let rec stack_rows j acc =
      if j >= height then acc
      else stack_rows (j+1) (List.init width ~f:(fun i -> get_color_at i j) :: acc)
    in
    List.rev @@ stack_rows 0 []
end
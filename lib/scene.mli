open Ray
open Vector
open Camera
open Light
open Shape
open Color

module Intersection_record : sig
  type t = {intersection_time: float; position: Vector3f.t; normal: Vector3f.t; }
end
module Scene : sig
  type t = {camera:Camera.t; lights:(module L) list; shapes:(module S) list}
  (* Gets the color at pixel (i,j), i.e. the pixel at the i-th row, j-th column *)
  val get_color : Ray.t -> i:int -> j:int -> Color.t
  val ray_trace : t -> width:int -> height:int -> rLimit:int -> cLimit:float -> Color.t list list
end
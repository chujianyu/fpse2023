open Ray

open Camera
open Light
open Shape
open Color





module Scene : sig

  type t = {camera:Camera.t; lights:(module L) list; shapes:(module S) list} 
  val to_string : t -> string
  val create : camera:Camera.t -> lights:(module L) list -> shapes:(module S) list -> t

  (* Gets the color at pixel (i,j), i.e. the pixel at the i-th row, j-th column *)
  val get_first_intersection : Ray.t -> (module S) list -> Intersection_record.t option
  val get_color : t -> Ray.t -> i:int -> j:int -> rLimit:int -> Color.t
  val ray_trace : t -> width:int -> height:int -> rLimit:int -> cLimit:float -> Color.t list list
end
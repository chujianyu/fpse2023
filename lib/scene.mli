open Ray

open Camera
open Light
open Shape
open Color


(* Scene module. Represents the 3D scene in which ray tracing can be performed *)
module Scene : sig

  type t = {camera:Camera.t; lights:(module L) list; shapes:(module S) list}

  (* Converts the scene to a string representation ;
    not using sexp because "(module S) type unsupported for ppx [of_sexp] conversion" *)
  val to_string : t -> string

  (* Creates a scene given the necessary elements*)
  val create : camera:Camera.t -> lights:(module L) list -> shapes:(module S) list -> t

  (* Gets the color of the  pixel (i,j), i.e. the pixel at the i-th row, j-th column *)
  val get_color : t -> Ray.t -> i:int -> j:int -> rLimit:int -> Color.t

  (* Performs ray tracing on the entire scene *)
  val ray_trace : t -> width:int -> height:int -> rLimit:int -> cLimit:float -> Color.t list list
end
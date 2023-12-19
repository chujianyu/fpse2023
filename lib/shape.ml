(* module Sphere and Shape.S may be used to create first-class modules*)
open Vector
open Sexplib.Std

module Material = struct 
  type t = {ambient : Color.t; diffuse : Color.t; specular : Color.t; emissive : Color.t; transparent : Color.t; shininess : float; ir:float} 
  [@@deriving sexp]
end
module Intersection = struct
  type t = {intersection_time: float; position: Vector3f.t; normal: Vector3f.t; material : Material.t} 
  [@@deriving sexp]
end
module Vertex =
  struct
    type t = 
      {
        pos : Vector3f.t;
        normal : Vector3f.t;
      } [@@deriving sexp]
  end

module type S = 
  sig
    type t  [@@deriving sexp]
    val item : t 
    val intersect : ray:Ray.t -> Intersection.t option
  end

(* module Sphere and Shape.S may be used to create first-class modules*)
open Ray
open Vector
open Color
open Sexplib.Std

module Material = struct 
  type t = {ambient : Color.t; diffuse : Color.t; specular : Color.t; emissive : Color.t; transparent : Color.t; shininess : float} 
  [@@deriving sexp]
end
module Intersection_record = struct
  type t = {intersection_time: float; position: Vector3f.t; normal: Vector3f.t; material : Material.t} 
  [@@deriving sexp]
end
module Vertex =
  struct
    type t = 
      {
        pos : Vector3f.t;
        normal : Vector3f.t;
      } 
      [@@deriving sexp]
  end

module Triangle_params =
  struct
    type t = 
      {
        v1 : Vertex.t;
        v2 : Vertex.t;
        v3 : Vertex.t;
        material : Material.t
      } 
      [@@deriving sexp]
  end


module Sphere_params = struct
  type t =
    {
      center : Vector3f.t;
      radius : float;
      material : Material.t
    } 
    [@@deriving sexp]
end 


module type S = 
  sig
    type t 
    val item : t 
    (* val create : Shape_create_params.t -> (t, string) result *)
    val intersect : ray:Ray.t -> Intersection_record.t option
    (* val normal_at : pos:Vector3f.t -> Vector3f.t *)
    val sexp_of_t : t -> Sexplib0.Sexp.t
    val t_of_sexp : Sexplib0.Sexp.t -> t
  end

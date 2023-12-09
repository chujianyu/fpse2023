open Ray
open Vector
open Color

(* Material module. Represents the material of a shape *)
module Material : sig
  type t = {ambient : Color.t; diffuse : Color.t; specular : Color.t; emissive : Color.t; transparent : Color.t; shininess : float} 
  (* [@@deriving sexp] *)
end

(* Intersection_record module. Represents the information about a ray-shape intersection *)
module Intersection_record : sig
  type t = {intersection_time: float; position: Vector3f.t; normal: Vector3f.t; material : Material.t} 
  (* [@@deriving sexp] *)
end

(* Vertex module. Represents the vertex of a shape, e.g. of triangles *)
module Vertex :
  sig
    type t = 
      {
        pos : Vector3f.t;
        normal : Vector3f.t;
      } 
      (* [@@deriving sexp] *)
  end

(* Triagnle_params module. Represents the parameters necessary used to create a triangle*)
module Triangle_params :
  sig
    type t = 
      {
        v0 : Vertex.t;
        v1 : Vertex.t;
        v2 : Vertex.t;
        material : Material.t
      } 
      [@@deriving sexp]
  end

(* Sphere_params module. *)
module Sphere_params :
sig
  type t =
    {
      center : Vector3f.t;
      radius : float;
      material : Material.t
    } 
    [@@deriving sexp]
end 


(* Shared module type for s shape. Designed to support use of first-class modules *)
module type S = 
  sig
    type t [@@deriving sexp]
    val item : t 
    val intersect : ray:Ray.t -> Intersection_record.t option
  end
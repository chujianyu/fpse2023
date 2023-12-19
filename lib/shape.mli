open Vector

(* Material module. Represents the material of a shape *)
module Material : sig
  type t = {ambient : Color.t; diffuse : Color.t; specular : Color.t; emissive : Color.t; 
  transparent : Color.t; shininess : float; ir : float} [@@deriving sexp]
end

(* Intersection_record module. Represents the information about a ray-shape intersection *)
module Intersection : sig
  type t = {intersection_time: float; position: Vector3f.t; normal: Vector3f.t; material : Material.t} 
  [@@deriving sexp]
end

(* Vertex module. Represents the vertex of a shape, e.g. of triangles *)
module Vertex :
  sig
    type t = 
      {
        pos : Vector3f.t;
        normal : Vector3f.t;
      } [@@deriving sexp]
  end


(* Shared module type for s shape. Designed to support use of first-class modules *)
module type S = 
  sig
    type t [@@deriving sexp]
    val item : t 
    val intersect : ray:Ray.t -> Intersection.t option
  end
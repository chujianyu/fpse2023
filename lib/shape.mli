(* module Sphere and Shape.S may be used to create first-class modules*)
open Ray
open Vector

module Vertex :
  sig
    type t = 
      {
        pos : Vector3f.t;
        normal : Vector3f.t;
      }
  end

module Triangle_params :
  sig
    type t = 
      {
        v1 : Vertex.t;
        v2 : Vertex.t;
        v3 : Vertex.t;
      }
  end


module Sphere_params :
sig
  type t =
    {
      center : Vector3f.t;
      radius : float;
    }
end 
(* module Shape_create_params : 
  sig
    type t =
    | Sphere_params of Sphere_params.t
    | Triangle_params of Triangle_params.t
  end *)
module type S = 
  sig
    type t
    val item : t
    (* val create : Shape_create_params.t -> (t, string) result *)
    val intersect : ray:Ray.t -> float option
    val normal_at : pos:Vector3f.t -> Vector3f.t
  end

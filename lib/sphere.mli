
open Shape
open Vector

(* Sphere_params module. *)
module Sphere_params :
sig
  type t =
    {
      center : Vector3f.t;
      radius : float;
      material : Material.t
    } [@@deriving sexp]
end 


(* function to make a sphere module *)
val make_sphere : Sphere_params.t -> (module Shape.S)
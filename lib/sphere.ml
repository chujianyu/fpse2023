[@@@warning "-69-27"]
open Vector
open Ray
open Shape

module Sphere = 
struct
  type t = Sphere_params.t
  let create param = match param with
  | Shape_create_params.Sphere_params {center : Vector3f.t; radius : float} -> 
    (* TODO: *) Ok Sphere_params.{center ; radius}
  | _ -> Error "Ill-formed param for sphere."

  let intersect sphere ~(ray:Ray.t) = None

  let normal_at sphere ~pos = Vector3f.empty ()
end
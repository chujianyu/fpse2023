[@@@warning "-69-27"]
open Vector
open Ray

module Sphere = 
struct
  type t = {center:Vector3f.t; radius:float}

  let create ~center ~radius = {center; radius}
  let intersect sphere ~(ray:Ray.t) = None

  let normal_at sphere ~pos = Vector3f.empty ()
end
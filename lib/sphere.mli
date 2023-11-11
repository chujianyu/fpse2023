open Vector 
open Ray
module Sphere : sig
  type t
  val create : center:Vector3f.t -> radius:float -> t
  val intersect : t -> ray:Ray.t -> float option
end
open Vector 
open Ray
module Sphere : sig
  type t
  val create : center:Vector3f.t -> radius:float -> t
  val intersect : t -> ray:Ray.t -> float option
  val normal_at : t -> pos:Vector3f.t -> Vector3f.t
end
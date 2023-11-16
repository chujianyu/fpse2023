[@@@warning "-69-27-33"]

open Ray
open Vector

module Point_light_param : sig
  type t = {pos:Vector3f.t; const_atten:float; linear_atten:float; quad_atten:float}
end

module type L = sig
  type t
  val item : t
  val getDiffuse : Ray.t -> Vector3f.t
end
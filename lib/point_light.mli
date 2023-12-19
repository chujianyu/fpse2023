open Light
open Vector
module Point_light_param : sig
  type t = {pos:Vector3f.t; ambient:Color.t; diffuse:Color.t; specular:Color.t; 
  const_atten:float; linear_atten:float; quad_atten:float} [@@deriving sexp]
end

val make_point_light : Point_light_param.t -> (module L)
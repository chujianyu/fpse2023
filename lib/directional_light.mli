open Light
open Vector

module Directional_light_param : sig
  type t = { dir:Vector3f.t; ambient:Color.t; diffuse:Color.t; specular:Color.t}
   [@@deriving sexp]
end

val make_directional_light : Directional_light_param.t -> (module L)
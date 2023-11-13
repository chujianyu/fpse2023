open Ray
open Vector

module Scene : sig
    
  type t
  val get_color : t -> Ray.t -> Vector3f.t
  val ray_trace : t -> unit
end
open Ray
open Vector

module Scene : sig
  type intersect_record_t = {t: float; position: Vector3f.t; normal: Vector3f.t; }
  type t
  val get_color : t -> Ray.t -> Vector3f.t
  val ray_trace : t -> unit
end
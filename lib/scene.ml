[@@@warning "-69-27-33"]

open Ray
open Vector

open Camera
open Light
open Shape

module Scene = struct
  type intersect_record_t = {intersection_time: float; position: Vector3f.t; normal: Vector3f.t; }
  type global_data_t = {camera:Camera.t; lights:(module L) list; shapes:(module S) list}
  type t
  let get_color _ _ _ =  Vector3f.empty ()
  let ray_trace  _ = [[]]
end
open Light
open Vector
let make_point_light (p : Point_light_param.t) = (module struct
  type t = Point_light_param.t
  let item = p
  let getDiffuse _ = Vector3f.empty ()
end : L)
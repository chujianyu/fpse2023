[@@@warning "-69-27-33-39"]
open Light
open Vector
open Color
open Ray
open Core
let make_point_light (p : Point_light_param.t) = (module struct
  type t = Point_light_param.t 
  [@@deriving sexp]
  let item = p

  let get_diffuse ray intersection normal material = 
    let open Vector3f in
    let i_to_light = item.pos -: intersection in
    let dist = norm i_to_light in
    let atten = item.const_atten +. item.linear_atten *. dist +. item.quad_atten *. dist *. dist in
    let open Float in
    if (dot i_to_light normal > 0.) && (dot normal (Ray.get_dir ray) < 0.) then
      Color.scale item.diffuse  ((dot (normalize i_to_light) (normalize normal)) /. atten)
    else Color.empty
end : L)
[@@@warning "-69-27-33-39"]
open Light
open Vector
open Color
open Ray
open Core
open Shape
let make_point_light (p : Point_light_param.t) = (module struct
  type t = Point_light_param.t 
  [@@deriving sexp]
  let item = p

  let get_ambient ray intersection normal material = 
    item.ambient


  let get_diffuse ray intersection normal material = 
    let open Vector3f in
    let i_to_light = item.pos -: intersection in
    let dist = norm i_to_light in
    let atten = item.const_atten +. item.linear_atten *. dist +. item.quad_atten *. dist *. dist in
    let open Float in
    if (dot i_to_light normal > 0.) && (dot normal (Ray.get_dir ray) < 0.) then
      Color.scale item.diffuse  ((dot (normalize i_to_light) (normalize normal)) /. atten)
    else Color.empty


  let get_specular ray intersection normal (material:Material.t) = 
    let open Vector3f in
    let i_to_light = normalize (item.pos -: intersection) in
    let dist = norm i_to_light in
    let atten = item.const_atten +. item.linear_atten *. dist +. item.quad_atten *. dist *. dist in
    let reflected_dir = Vector3f.normalize @@ reflect (Vector3f.scale (Ray.get_dir ray) (-1.)) normal in
    let open Float in
    if (dot i_to_light reflected_dir > 0.) && (dot normal (Ray.get_dir ray) < 0.) then
      let intensity = (dot i_to_light reflected_dir) ** material.shininess / atten in
      Color.scale item.specular intensity
    else Color.empty
  
    
end : L)
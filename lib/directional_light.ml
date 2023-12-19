[@@@warning "-69-27-33-39"]
open Light
open Vector
open Color
open Ray
open Core
open Shape
let make_directional_light (p : Directional_light_param.t) = (module struct
  type t = Directional_light_param.t 
  [@@deriving sexp]
  let item = p

  let get_ambient _ray _intersection _normal material =
    item.ambient

  let get_diffuse ray intersection normal material =
    let open Vector3f in
    let open Float in 
    let to_light = Vector3f.normalize @@ Vector3f.scale item.dir (-1.) in 
    if (dot to_light normal) > 0. && dot (Ray.get_dir ray) normal < 0. then
      let intensity = dot to_light normal in
      item.diffuse |> Fn.flip Color.scale intensity
    else
      Color.empty

  let get_specular ray intersection normal (material:Material.t) =
    let open Vector3f in
    let open Float in
    let to_light = Vector3f.normalize @@ Vector3f.scale item.dir (-1.) in
    let ref_dir = Vector3f.normalize @@ Vector3f.reflect (Vector3f.scale (Ray.get_dir ray) (-1.)) normal in
    if (dot to_light normal) > 0. && dot (Ray.get_dir ray) normal < 0. then
      let intensity = dot ref_dir to_light in
      item.specular |> Fn.flip Color.scale (intensity ** material.shininess)
    else
      Color.empty
    
end : L)
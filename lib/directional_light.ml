open Light
open Vector
open Core
open Shape

let epsilon = 0.0001

module Directional_light_param = struct
  type t = { dir:Vector3f.t; ambient:Color.t; diffuse:Color.t; specular:Color.t}
   [@@deriving sexp]
end

let make_directional_light (p : Directional_light_param.t) = (module struct
  type t = Directional_light_param.t 
  [@@deriving sexp]
  let item = p

  let get_ambient _ray _intersection _normal _material =
    item.ambient

  let get_diffuse ray _intersection normal _material =
    let open Vector3f in
    let open Float in 
    let to_light = Vector3f.normalize @@ Vector3f.scale item.dir (-1.) in 
    if (dot to_light normal) > 0. && dot (Ray.get_dir ray) normal < 0. then
      let intensity = dot to_light normal in
      item.diffuse |> Fn.flip Color.scale intensity
    else
      Color.empty

  let get_specular ray _ normal (material:Material.t) =
    let open Vector3f in
    let open Float in
    let to_light = Vector3f.normalize @@ Vector3f.scale item.dir (-1.) in
    let ref_dir = Vector3f.normalize @@ Vector3f.reflect (Vector3f.scale (Ray.get_dir ray) (-1.)) normal in
    if (dot to_light normal) > 0. && dot (Ray.get_dir ray) normal < 0. then
      let intensity = dot ref_dir to_light in
      item.specular |> Fn.flip Color.scale (intensity ** material.shininess)
    else
      Color.empty

  let transparency intersection shapes cLimit =
    let ray_direction = Vector3f.scale item.dir (-1.) in
    (*Add offset to account for floating point errors*)
    let ray_orig = Vector3f.add intersection (Vector3f.scale ray_direction epsilon) in
    let ray = Ray.create ~orig:ray_orig ~dir:(Vector3f.normalize ray_direction) in
    let rec accumulate_transparency shapes trans =
      match shapes with
      | [] -> trans
      | (module Shape : S) :: rest_shapes ->
        let intersection_info = Shape.intersect ~ray in
        match intersection_info with
        | None -> accumulate_transparency rest_shapes trans 
        | Some iInfo ->
          let new_trans = Color.mul trans iInfo.material.transparent in
          if Color.greater new_trans cLimit then
            accumulate_transparency rest_shapes new_trans 
          else 
            new_trans
    in
    accumulate_transparency shapes (Color.make ~r:1.0 ~g:1.0 ~b:1.0) 
    
end : L)
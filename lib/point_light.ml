open Light
open Vector
open Core
open Shape

module Point_light_param = struct
  type t = {pos:Vector3f.t; ambient:Color.t; diffuse:Color.t; specular:Color.t; const_atten:float; linear_atten:float; quad_atten:float}
   [@@deriving sexp]
end

let make_point_light (p : Point_light_param.t) = (module struct
  type t = Point_light_param.t 
  [@@deriving sexp]
  let item = p

  let get_ambient _ray _intersection _normal _material = 
    item.ambient


  let get_diffuse ray intersection normal _material = 
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

  let transparency intersection shapes cLimit =
    let ray_direction = Vector3f.subtract item.pos intersection in
    let distance_to_light = Vector3f.norm ray_direction in
    let ray_orig = Vector3f.add intersection (Vector3f.scale ray_direction 0.0001) in
    let ray = Ray.create ~orig:ray_orig ~dir:(Vector3f.normalize ray_direction) in

    let rec accumulate_transparency shapes trans distance =
      match shapes with
      | [] -> trans
      | (module Shape : S) :: rest_shapes ->
        let intersection_info = Shape.intersect ~ray in
        match intersection_info with
        | None -> accumulate_transparency rest_shapes trans distance
        | Some iInfo ->
          let open Float in
          if iInfo.intersection_time > distance then 
            accumulate_transparency rest_shapes trans distance
          else
            let new_trans = Color.mul trans iInfo.material.transparent in
            if Color.greater new_trans cLimit then
              accumulate_transparency rest_shapes new_trans distance
            else 
              new_trans
    in
    accumulate_transparency shapes (Color.make ~r:1.0 ~g:1.0 ~b:1.0) distance_to_light

  
    
end : L)
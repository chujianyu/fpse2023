
[@@@warning "-69-27-33-32"]
open Vector
open Yojson.Basic.Util
open Shape
open Light
open Scene
open Color
open Camera

(* Module to parse parse the input json file as given in example_input/ *)
module Parse = struct

let parse_color json =
  match json with
  | `List [r; g; b] -> Color.make ~r:(to_float r) ~g:(to_float g) ~b:(to_float b)
  | _ -> failwith "Invalid color format"
  
let parse_vector json = 
  match json with
  | `List [x; y; z] -> Vector3f.create ~x:(to_float x) ~y:(to_float y) ~z:(to_float z)
  | _ -> failwith "Invalid vector format"

let parse_material json = 
  {
    Material.ambient = parse_color (member "ambient" json);
    shininess = member "shininess" json |> to_float;
    diffuse = parse_color (member "diffuse" json);
    specular = parse_color (member "specular" json);
    emissive = parse_color (member "emissive" json);
    transparent = parse_color (member "transparent" json);
  }

let parse_sphere json = 
  let params = {
    Sphere_params.center = parse_vector (member "center" json);
    radius = member "radius" json |> to_float;
    material = parse_material (member "material" json)}
  in
  Sphere.make_sphere params

let parse_point_light json = 
  let pos = json |> member "pos" |> parse_vector in
  let diffuse = json |> member "diffuse" |> parse_color in
  let const_atten = json |> member "constAtten" |> to_float in
  let linear_atten = json |> member "linearAtten" |> to_float in
  let quad_atten = json |> member "quadAtten" |> to_float in
  let params = {
    Point_light_param.pos = pos;
    diffuse = diffuse;
    const_atten = const_atten;
    linear_atten = linear_atten;
    quad_atten = quad_atten;} 
  in
  Point_light.make_point_light params

let parse_camera json = 
  let height_angle = json |> member "heightAngle" |> to_float in
  let pos = json |> member "origin" |> parse_vector in
  let up = json |> member "up" |> parse_vector in
  let forward = json |> member "forward" |> parse_vector in
  Camera.create ~height_angle ~pos ~up ~forward
  


let parse_scene filename =
  let json = Yojson.Basic.from_file filename in
  let spheres = json |> member "spheres" |> to_list |> List.map parse_sphere in
  let point_lights = json |> member "pointLights" |> to_list |> List.map parse_point_light in
  let camera = json |> member "camera" |> parse_camera in
  Scene.create ~shapes:(spheres@[]) ~lights:(point_lights@[]) ~camera

end
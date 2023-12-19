
[@@@warning "-69-27-33-32"]
open Vector
open Yojson.Basic.Util
open Shape
open Light
open Scene
open Color
open Camera
open Core

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
    ir = member "ir" json |> to_float;
  }

let parse_vertex json =
  {
    Vertex.pos = parse_vector (member "pos" json);
    normal = parse_vector (member "norm" json)
  }

let parse_triangle json = 
  let params = {
    Triangle_params.v0 = parse_vertex (member "v0" json);
    v1 = parse_vertex (member "v1" json);
    v2 = parse_vertex (member "v2" json);
    material = parse_material (member "material" json)}
  in
  Triangle.make_triangle params

let parse_sphere json = 
  let params = {
    Sphere_params.center = parse_vector (member "center" json);
    radius = member "radius" json |> to_float;
    material = parse_material (member "material" json)}
  in
  Sphere.make_sphere params

let parse_point_light json = 
  let pos = json |> member "pos" |> parse_vector in
  let ambient = json |> member "ambient" |> parse_color in
  let diffuse = json |> member "diffuse" |> parse_color in
  let specular = json |> member "specular" |> parse_color in
  let const_atten = json |> member "constAtten" |> to_float in
  let linear_atten = json |> member "linearAtten" |> to_float in
  let quad_atten = json |> member "quadAtten" |> to_float in
  let params = {
    Point_light_param.pos = pos;
    ambient = ambient;
    diffuse = diffuse;
    specular = specular;
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

let parse_sky_enabled json =
  json |> member "skyEnabled" |> to_bool_option
  
  


let parse_scene filename =
  let json = Yojson.Basic.from_file filename in
  let spheres = json |> member "spheres" |> to_list |> List.map ~f:parse_sphere in
  let triangles = json |> member "triangles" |> to_list |> List.map ~f:parse_triangle in
  let point_lights = json |> member "pointLights" |> to_list |> List.map ~f:parse_point_light in
  let camera = json |> member "camera" |> parse_camera in
  let sky_enabled = json |> parse_sky_enabled |> Option.value ~default:false in
  Scene.create ~shapes:(spheres@triangles) ~lights:(point_lights@[]) ~camera ~sky_enabled

end
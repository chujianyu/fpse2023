[@@@warning "-69-27-33-39-32"]

open Ray
open Vector

open Camera
open Light
open Shape
open Core
open Color


module Scene = struct

  type t = {camera:Camera.t; lights:(module L) list; shapes:(module S) list}

  let to_string scene =
    let open Sexplib0.Sexp_conv in
    let lights_sexp = List.map scene.lights ~f:(fun (module Light : L) ->
      Light.sexp_of_t Light.item
    ) in
    let shapes_sexp = List.map scene.shapes ~f:(fun (module Shape : S) ->
      Shape.sexp_of_t Shape.item
    ) in
    let camera_sexp = Camera.sexp_of_t scene.camera in
    let lights_str = String.concat ~sep:"\n" (List.map ~f:Sexp.to_string_hum lights_sexp) in
    let shapes_str = String.concat ~sep:"\n" (List.map ~f:Sexp.to_string_hum shapes_sexp) in
    let camera_str = Sexp.to_string_hum camera_sexp in
    Printf.sprintf "Scene:\nLights:\n%s\nShapes:\n%s\nCamera:\n%s\n" lights_str shapes_str camera_str

  let create ~camera ~lights ~shapes = 
    let scene = {camera; lights; shapes} in
    print_endline @@ to_string scene;
    scene
  let get_light_contribution ray (lights:(module L) list) ({intersection_time; position; normal; material}:Intersection_record.t) : Color.t = 
   
    let f_accumulate acc light_module =
      let module Cur_light = (val light_module : L) in
      let diffuse = Color.mul (Cur_light.get_diffuse ray position normal material) material.diffuse in
      
      (* let specular = Color.mul (Cur_light.get_specular ray intersection) intersection.material.specular in *)
      let total = Color.add diffuse Color.empty in
      Color.add acc total
    in
    List.fold lights ~init:Color.empty ~f:f_accumulate

  


  let get_first_intersection ray shapes =
    let get_closer_intersection (closer:Intersection_record.t option) (new_intersection:Intersection_record.t option) = 
      match closer, new_intersection with
      | None, None -> None
      | None, Some _ -> new_intersection
      | Some _, None -> closer
      | Some i1, Some i2 -> 
        if Float.(<=) i1.intersection_time i2. intersection_time then closer else new_intersection
    in
    let f_keep_closest closest shape_module =
      let module Cur_shape = (val shape_module : S) in
      let new_intersection = Cur_shape.intersect ~ray in
      get_closer_intersection closest new_intersection
    in
    List.fold shapes ~init:None ~f:f_keep_closest

    (* TODO: remove unused params *)
    (*  let rec get_color {lights; shapes; _} ray ~i ~j ~rLimit  = *)
  let rec get_color {camera; lights; shapes} ray ~i ~j ~rLimit  = 
  match rLimit with 
  | 0 -> Color.empty
  | _ ->
    match get_first_intersection ray shapes with
    | None -> Color.empty
    | Some intersect -> 
      let emission = intersect.material.emissive in  
      let light_contribution = get_light_contribution ray lights intersect |> Color.add emission in

      let hit_front_face =  Vector3f.dot (Ray.get_dir ray) (intersect.normal) in 
      if Core.Float.(<.) hit_front_face 0.  then 
        let reflect_dir = Vector3f.reflect ( Vector3f.scale (Ray.get_dir ray) (-1.0) ) (intersect.normal) in 
        let reflect_pos = (Vector3f.add (intersect.position)  (Vector3f.scale reflect_dir 0.01)) in 
        let reflect_ray = Ray.create ~orig:reflect_pos ~dir:reflect_dir in

        (*get_color {camera; lights; shapes} (Camera.get_ray camera ~i ~j ~width ~height) ~i ~j ~rLimit*)
        let ref_light_contribution =Color.scale (Color.mul (get_color {camera; lights; shapes} reflect_ray ~i:i ~j:j ~rLimit:(rLimit-1)) (intersect.material.specular)) (1.0) in
        let updated_light_contribution = Color.add ref_light_contribution light_contribution in 
        (*let v1 = Vector3f.create ~x:10.0 ~y:10.0 ~z:0.0 in 
        let v2 = Vector3f.create ~x:0.0 ~y:10.0 ~z:0.0 in 
        let v3 = Vector3f.reflect v1 v2 in 
        print_endline *)
        
(*
   ouit 2 seq fold left 
   assert command 
   see txt
   test name see txt
*)
        updated_light_contribution
      else
      
      (* let recur_contribution =  *)
      light_contribution
    
  let ray_trace {camera; lights; shapes} ~width ~height ~rLimit ~cLimit : Color.t list list = 
    let get_color_at i j =
      get_color {camera; lights; shapes}  (Camera.get_ray camera ~i ~j ~width ~height) ~i ~j ~rLimit
    in
    let rec stack_rows i acc =
      if i >= height then acc
      else stack_rows (i+1) (List.init width ~f:(fun j -> get_color_at i j) :: acc)
    in
    stack_rows 0 []


module T = Domainslib.Task

let ray_trace_parallel {camera; lights; shapes} ~width ~height ~rLimit ~cLimit ~pool : Color.t list list = 
let get_color_at i j =
  get_color {camera; lights; shapes} (Camera.get_ray camera ~i ~j ~width ~height) ~i ~j ~rLimit
in

(* Create a 2D array *)
let colors = Array.make_matrix ~dimx:height ~dimy:width Color.empty in

(* Use parallel_for to fill the array *)
T.parallel_for pool ~start:0 ~finish:(height - 1)
  ~body:(fun i ->
    for j = 0 to width - 1 do
      colors.(height-1-i).(j) <- get_color_at i j
    done
  );

(* Convert the 2D array to a 2D list *)
Array.to_list (Array.map ~f:Array.to_list colors)

let ray_trace {camera; lights; shapes} ~width ~height ~rLimit ~cLimit : Color.t list list = 
let num_domains = 20 in 
let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
let result = T.run pool (fun () -> ray_trace_parallel {camera; lights; shapes} ~width ~height ~rLimit ~cLimit ~pool) in
T.teardown_pool pool;
result

end

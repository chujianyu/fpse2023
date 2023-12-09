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
      let ambient = Color.mul (Cur_light.get_ambient ray position normal material) material.ambient in
      let diffuse = Color.mul (Cur_light.get_diffuse ray position normal material) material.diffuse in
      let specular = Color.mul (Cur_light.get_specular ray position normal material) material.specular in
      List.fold ~f:Color.add ~init:Color.empty [acc;ambient;diffuse;specular]
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
  let rec get_color {camera; lights; shapes} ray ~i ~j ~rLimit ~(cLimit:Color.t) = 
  match rLimit with 
  | 0 -> Color.empty
  | _ ->
    match get_first_intersection ray shapes with
    | None -> Color.empty
    | Some intersect -> 
      let emissive = intersect.material.emissive in
      let light_contribution = get_light_contribution ray lights intersect in
      let reflection_contribution =
        if not @@ Color.greater intersect.material.specular cLimit then Color.empty
        else
          let hit_front_face =  Vector3f.dot (Ray.get_dir ray) (intersect.normal) in 
          if Core.Float.(>.) hit_front_face 0. then Color.empty
          else
            (* Multiplying incident ray.dir by -1 to get outgoing direction *)
            let reflect_dir = Vector3f.reflect ( Vector3f.scale (Ray.get_dir ray) (-1.0) ) (intersect.normal) in 
            let reflect_pos = (Vector3f.add (intersect.position)  (Vector3f.scale reflect_dir 0.01)) in 
            let reflect_ray = Ray.create ~orig:reflect_pos ~dir:reflect_dir in
            (* cutoff to filter out minimal contribution for early stopping *)
            let cLimit = Color.div cLimit intersect.material.specular in
            Color.scale (Color.mul (get_color {camera; lights; shapes} reflect_ray ~i:i ~j:j ~rLimit:(rLimit-1) ~cLimit) (intersect.material.specular)) (1.0) 
      in
      (* TODO: let refract_contrib = ... *)
      let color_contributions = [emissive; light_contribution; reflection_contribution] in
      List.fold ~f:Color.add ~init:Color.empty color_contributions
    
  let ray_trace {camera; lights; shapes} ~width ~height ~rLimit ~cLimit : Color.t list list = 
    let get_color_at i j =
      get_color {camera; lights; shapes}  (Camera.get_ray camera ~i ~j ~width ~height) ~i ~j ~rLimit ~cLimit:(Color.make ~r:cLimit ~g:cLimit ~b:cLimit)
    in
    let rec stack_rows i acc =
      if i >= height then acc
      else stack_rows (i+1) (List.init width ~f:(fun j -> get_color_at i j) :: acc)
    in
    stack_rows 0 []




let num_cores () =
  let command = "getconf _NPROCESSORS_ONLN" in
  let in_channel = Caml_unix.open_process_in command in
  let line = In_channel.input_line in_channel in
  In_channel.close in_channel;
  match line with
  | Some n -> Int.of_string n
  | None -> 1
  
(* A parallelization attempt with arrays; This is the only mutation happening during ray tracing; 
we will change to use list with chunking and combining them 
to achieve parallelization with immutable data structure. *)
module T = Domainslib.Task
let ray_trace_parallel {camera; lights; shapes} ~width ~height ~rLimit ~cLimit ~pool : Color.t list list = 
  let get_color_at i j =
    get_color {camera; lights; shapes} (Camera.get_ray camera ~i ~j ~width ~height) ~i ~j ~rLimit ~cLimit:(Color.make ~r:cLimit ~g:cLimit ~b:cLimit)
  in
  let colors = Array.make_matrix ~dimx:height ~dimy:width Color.empty in
  T.parallel_for pool ~start:0 ~finish:(height - 1)
    ~body:(fun i ->
      for j = 0 to width - 1 do
        colors.(height-1-i).(j) <- get_color_at i j
      done
    );
  Array.to_list (Array.map ~f:Array.to_list colors)

let ray_trace {camera; lights; shapes} ~width ~height ~rLimit ~cLimit : Color.t list list = 
  (* Number of cpu cores. Using half of the cores now, as setting it higher
     than what's available can negatively impact performance *)
  let num_domains = num_cores () / 2 in 
  print_endline @@ Printf.sprintf "Using %d cores" num_domains;
  let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
  let result = T.run pool (fun () -> ray_trace_parallel {camera; lights; shapes} ~width ~height ~rLimit ~cLimit ~pool) in
  T.teardown_pool pool;
  result

end

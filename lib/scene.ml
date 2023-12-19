

(*https://raytracing.github.io/books/RayTracingInOneWeekend.html*)
open Vector
open Light
open Shape
open Core




type t = {camera:Camera.t; lights:(module L) list; shapes:(module S) list; sky_enabled:bool}

let to_string scene =
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

let create ~camera ~lights ~shapes ~sky_enabled= 
  let scene = {camera; lights; shapes; sky_enabled} in
  print_endline @@ to_string scene;
  scene
let get_light_contribution ray (lights:(module L) list) ({position; normal; material; _}:Intersection_record.t) : Color.t = 
  let f_accumulate acc light_module =
    let module Cur_light = (val light_module : L) in  
    let ambient = Color.mul (Cur_light.get_ambient ray position normal material) material.ambient in
    let diffuse = Color.mul (Cur_light.get_diffuse ray position normal material) material.diffuse in
    let specular = Color.mul (Cur_light.get_specular ray position normal material) material.specular in
    List.fold ~f:Color.add ~init:Color.empty [acc;ambient;diffuse;specular]
  in
  List.fold lights ~init:Color.empty ~f:f_accumulate



  let refract ray_dir normal ir =
    let open Float in
    let reverse_ray_dir = Vector3f.scale ray_dir (-1.0) in
    let dot = Vector3f.dot reverse_ray_dir normal in
    let ir = if Float.(>) dot 0. then 1.0/.ir else ir in
    let in_cos = Float.min 1.0 @@ Float.abs dot in
    let in_sin = Float.sqrt @@ 1. -. in_cos *. in_cos in  
    let out_sin = in_sin *. ir in
    if out_sin > 1. then None
    else 
      let out_cos = Float.sqrt @@ 1. -. out_sin *. out_sin in
      let first_component = Vector3f.scale normal (ir *. in_cos -. out_cos) in
      let second_component = 
        if dot >= 0. then Vector3f.scale ray_dir ir 
        else Vector3f.scale reverse_ray_dir ir
      in
      if dot >= 0. then Some (Vector3f.add first_component second_component)
      else Some (Vector3f.scale (Vector3f.add first_component second_component) (-1.))

      


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
let rec get_color {camera; lights; shapes; sky_enabled} ray ~i ~j ~rLimit ~(cLimit:Color.t) = 
match rLimit with 
| 0 -> Color.empty
| _ ->
  match get_first_intersection ray shapes with
  | None -> if not sky_enabled then Color.empty 
    else (* compute sky color *)
    (
      let direction = Ray.get_dir ray in 
      let dir_unit = Vector3f.normalize direction in 
      let y_comp = Vector3f.to_tuple dir_unit in 
      match y_comp with 
      | (_,y,_) -> (
        let a = 0.5*.(y +. 1.0) in 
        Color.add (Color.scale (Color.make ~r:1.0 ~g:1.0 ~b:1.0 ) (1.0-.a))
        (Color.scale (Color.make ~r:0.5 ~g:0.7 ~b:1.0 ) (a))
      )
    )
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
          (*let reflect_pos = (Vector3f.add (intersect.position)  (Vector3f.scale reflect_dir 0.01)) in *)
          let reflect_pos = (Vector3f.add (intersect.position)  (Vector3f.scale intersect.normal 0.001)) in 
          let reflect_ray = Ray.create ~orig:reflect_pos ~dir:reflect_dir in
          (* cutoff to filter out minimal contribution for early stopping *)
          let cLimit = Color.div cLimit intersect.material.specular in
          Color.scale (Color.mul (get_color {camera; lights; shapes; sky_enabled} reflect_ray ~i:i ~j:j ~rLimit:(rLimit-1) ~cLimit) (intersect.material.specular)) (1.0) 
    in
    let refraction_contribution = 
      if not @@ Color.greater intersect.material.transparent cLimit then Color.empty
      else 
        match refract (Ray.get_dir ray) intersect.normal intersect.material.ir with
        | None -> Color.empty
        | Some refract_dir -> 
          let refract_pos = (Vector3f.add (intersect.position)  (Vector3f.scale refract_dir 0.00000001)) in 
          let refract_ray = Ray.create ~orig:refract_pos ~dir:refract_dir in
          let cLimit = Color.div cLimit intersect.material.transparent in
          Color.scale (Color.mul (get_color {camera; lights; shapes; sky_enabled} refract_ray ~i:i ~j:j ~rLimit:(rLimit-1) ~cLimit) (intersect.material.transparent)) (1.0)
    in

    let color_contributions = [emissive; light_contribution; reflection_contribution; refraction_contribution] in
    List.fold ~f:Color.add ~init:Color.empty color_contributions
  
(* Ray trace without parallelism for demonstration purposes *)
let ray_trace {camera; lights; shapes; sky_enabled} ~width ~height ~rLimit ~cLimit : Color.t list list = 
  let get_color_at i j =
    let samples = (Core.List.range ~stride:1 ~start:`inclusive
    ~stop:`inclusive 1 3) in 
    let multiplier = 1.0/.3.0 in
    Core.List.fold_left 
    samples 
    ~init:Color.empty 
    ~f:(fun acc _ -> (
      let sample_color = 
      Color.scale  ( get_color {camera; lights; shapes; sky_enabled}  (Camera.get_ray camera ~i ~j ~width ~height ~random:true) ~i ~j ~rLimit ~cLimit:(Color.make ~r:cLimit ~g:cLimit ~b:cLimit) )multiplier in 
      Color.add acc sample_color
    ))
    
  in
  let rec stack_rows i acc =
    if i >= height then acc
    else stack_rows (i+1) (List.init width ~f:(fun j -> get_color_at i j) :: acc)
  in
  stack_rows 0 []

(* A parallelization attempt with arrays; This is the only mutation happening during ray tracing; 
we will change to use list with chunking and combining them 
to achieve parallelization with immutable data structure. *)
module T = Domainslib.Task

let ray_trace_parallel {camera; lights; shapes; sky_enabled} ~width ~height ~rLimit ~cLimit ~num_domains ~pool =

  let get_color_at i j =
    get_color {camera; lights; shapes; sky_enabled} (Camera.get_ray camera ~i ~j ~width ~height ~random:true) ~i ~j ~rLimit ~cLimit:(Color.make ~r:cLimit ~g:cLimit ~b:cLimit)
  in
  (* Function to process a chunk of rows *)
  let process_chunk (start_row, end_row) =
    let rec stack_rows i acc =
      if i >= end_row then acc
      else stack_rows (i + 1) (List.init width ~f:(fun j -> get_color_at i j) :: acc)
    in
    stack_rows start_row []
  in

  (* Splitting the image into chunks *)
  let chunk_size = (height + num_domains - 1) / num_domains in
  let rec create_chunks start_row =
    if start_row >= height then []
    else let end_row = min height (start_row + chunk_size) in
         (start_row, end_row) :: create_chunks end_row
  in
  let chunks = List.rev @@ create_chunks 0 in

  (* Process each chunk in parallel and concatenate the results *)
    let tasks = List.map ~f:(fun chunk -> T.async pool (fun () -> process_chunk chunk)) chunks in
    let all_rows = List.concat (List.map ~f:(T.await pool) tasks) in
    all_rows

let ray_trace ?(num_domains=1) {camera; lights; shapes; sky_enabled} ~width ~height ~rLimit ~cLimit : Color.t list list = 
  print_endline @@ Printf.sprintf "Using %d domain(s)" num_domains;
  (* Could just use ray trace parallel with num_domains = 1, but keeping this for demonstration purposes and comparison *)
  if num_domains = 1 then ray_trace {camera; lights; shapes; sky_enabled} ~width ~height ~rLimit ~cLimit
  else
    let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
    let result = T.run pool (fun () -> ray_trace_parallel {camera; lights; shapes; sky_enabled} ~width ~height ~rLimit ~cLimit ~pool ~num_domains) in
    T.teardown_pool pool;
    result


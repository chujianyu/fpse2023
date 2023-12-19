
[@@@warning "-69-27-33-32"]
open Core
open Fpse2023_raytracer_lib
open Parse
open Scene
open Output

let timed_ray_trace scene ~width ~height ~rLimit ~cLimit ~num_domains = 
  let start_time = Time_float.now () in
  let img = Scene.ray_trace scene ~num_domains ~width ~height ~rLimit ~cLimit in
  let end_time = Time_float.now () in
  let duration = Time_float.diff end_time start_time in
  Printf.printf "Ray tracing completed in: %f seconds\n" (Time_float.Span.to_sec duration);
  img

let timed_write_file img ~width ~height ~out_filename = 
  let start_time = Time_float.now() in 
  let _ = Output.output_rgb_data_to_file img ~width ~height ~out_filename in 
  let end_time = Time_float.now() in 
  let duration = Time_float.diff end_time start_time in 
  Printf.printf "Writing output to file completed in: %f seconds\n" (Time_float.Span.to_sec duration)

let num_cores () =
  let command = "getconf _NPROCESSORS_ONLN" in
  let in_channel = Caml_unix.open_process_in command in
  let line = In_channel.input_line in_channel in
  In_channel.close in_channel;
  match line with
  | Some n -> Int.of_string n
  | None -> 1
    

let main ~in_filename ~out_filename ~width ~height ~rLimit ~cLimit ~num_domains =
  in_filename
  |> Parse.parse_scene
  |> timed_ray_trace ~width ~height ~rLimit ~cLimit ~num_domains
  |> timed_write_file ~width ~height ~out_filename
  
let validate_input_file filename =
  match Sys_unix.file_exists filename with
  | `No | `Unknown -> Error (`Msg "INPUT FILE not found")
  | `Yes -> 
    match Filename.split_extension filename with
    | _, Some "json" -> Ok ()
    | _ -> Error (`Msg "INPUT FILE must have a .json extension")

let validate_output_filename filename =
  match Filename.split_extension filename with
  | _, Some "ppm" -> Ok ()
  | _ -> Error (`Msg "OUTPUT FILE must have a .ppm extension")

let validate_dimensions width height =
  if width > 0 && height > 0 then Ok ()
  else Error (`Msg "WIDTH and HEIGHT must be both positive")

let validate_recursion_limit limit =
  if limit >= 0 then Ok ()
  else Error (`Msg "RECURSION LIMIT must be non-negative")

let validate_cut_off_threshold threshold =
  let open Float in
  if threshold > 0.0 && threshold <= 1.0 then Ok ()
  else Error (`Msg "CUTOFF THRESHOLD must be between 0 and 1")

let validate_domains domains =
  if domains < 0 then Error (`Msg "DOMAINS must be positive")
  (* Setting num_domains too high can negatively impact performance. 
     as noted in https://v2.ocaml.org/releases/5.0/manual/parallelism.html *)
  else if domains > num_cores () then Error (`Msg ("DOMAINS must be less than or equal to the number of cores: "^string_of_int (num_cores ())))
  else Ok ()
  

let command =
  Command.basic
    ~summary:"OCaml Ray Tracer"
    (let%map_open.Command
      in_filename = flag "--in" (required string) ~doc:"FILENAME INPUT JSON FILE NAME"
      and out_filename = flag "--out" (required string) ~doc:"FILENAME OUTPUT PPM FILE NAME"
      and width = flag "--width" (optional_with_default 640 int) ~doc:"WIDTH OUTPUT IMAGE WIDTH (default 640)"
      and height = flag "--height" (optional_with_default 480 int) ~doc:"HEIGHT OUTPUT IMAGE HEIGHT (default 480)"
      and recursion_limit = flag "--rLimit" (optional_with_default 5 int) ~doc:"NUM RECURSION DEPTH LIMIT (default 5)"
      and cut_off_threshold = flag "--cutOff" (optional_with_default 0.0001 float) 
        ~doc:"FLOAT CUTOFF THRESHOLD representing minimum color contribution (default 0.0001)"
      and num_domains = flag "--domains" (optional_with_default 1 int)
        ~doc:"NUM The NUM OF DOMAINS for Parallelism (default 1)"
    in
    fun () ->
      let validation_results = [
        validate_input_file in_filename;
        validate_dimensions width height;
        validate_recursion_limit recursion_limit;
        validate_cut_off_threshold cut_off_threshold;
        validate_output_filename out_filename;
        validate_domains num_domains] 
      in
      match List.find ~f:Result.is_error validation_results with
      | Some (Error (`Msg err)) -> eprintf "Error: %s\n" err
      | _ ->
        main ~in_filename ~out_filename ~width ~height ~rLimit:recursion_limit ~cLimit:cut_off_threshold ~num_domains)
        
let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command


[@@@warning "-69-27-33-32"]
open Core
open Fpse2023_raytracer_lib
open Parse
open Scene
open Output

let main ~in_filename ~out_filename ~width ~height ~rLimit ~cLimit =
  in_filename
  |> Parse.parse_scene
  |> Scene.ray_trace ~width ~height ~rLimit ~cLimit
  |> Output.output_rgb_data_to_file ~width ~height ~out_filename

  

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

let command =
  Command.basic
    ~summary:"OCaml Ray Tracer"
    (let%map_open.Command
      in_filename = flag "--in" (required string) ~doc:"INPUT JSON FILE NAME"
      and out_filename = flag "--out" (required string) ~doc:"OUTPUT FILE NAME"
      and width = flag "--width" (optional_with_default 640 int) ~doc:"OUTPUT IMAGE WIDTH (default 640)"
      and height = flag "--height" (optional_with_default 480 int) ~doc:"OUTPUT IMAGE HEIGHT (default 480)"
      and recursion_limit = flag "--rLimit" (optional_with_default 5 int) ~doc:"RECURSION DEPTH LIMIT (default 5)"
      and cut_off_threshold = flag "--cutOff" (optional_with_default 0.0001 float) 
        ~doc:"CUTOFF THRESHOLD representing minimum color contribution (default 0.0001)"
    in
    fun () ->
      match validate_input_file in_filename, validate_dimensions width height, validate_recursion_limit recursion_limit, validate_cut_off_threshold cut_off_threshold, validate_output_filename out_filename with
      | Ok (), Ok (), Ok (), Ok (), Ok () -> 
        main ~in_filename ~out_filename ~width ~height ~rLimit:recursion_limit ~cLimit:cut_off_threshold
      | Error (`Msg err), _, _, _, _ | _, Error (`Msg err), _, _, _ | _, _, Error (`Msg err), _, _ | _, _, _, Error (`Msg err), _ | _, _, _, _, Error (`Msg err)  -> eprintf "Error: %s\n" err)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

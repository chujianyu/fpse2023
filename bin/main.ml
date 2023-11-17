
[@@@warning "-69-27-33-32"]
open Core
open Fpse2023_raytracer_lib
open Parse
open Scene

(* let main in_filename out_filename width height () =
  in_filename
  |> Parse.parse
  |> Scene.ray_trace ~width ~height
  |> Output.write ~out_filename *)

  

let validate_input_file filename =
  match Sys_unix.file_exists filename with
  | `No | `Unknown -> Error (`Msg "INPUT FILE not found")
  | `Yes -> 
    match Filename.split_extension filename with
    | _, Some "json" -> Ok ()
    | _ -> Error (`Msg "INPUT FILE must have a .json extension")

let validate_dimensions width height =
  if width > 0 && height > 0 then Ok ()
  else Error (`Msg "WIDTH and HEIGHT must be both positive")

let command =
  Command.basic
    ~summary:"OCaml Ray Tracer"
    (let%map_open.Command
      in_filename = flag "--in" (required string) ~doc:"INPUT JSON FILE NAME"
      and out_filename = flag "--out" (required string) ~doc:"OUTPUT FILE NAME"
      and width = flag "--width" (optional_with_default 640 int) ~doc:"OUTPUT IMAGE WIDTH (default 640)"
      and height = flag "--height" (optional_with_default 480 int) ~doc:"OUTPUT IMAGE HEIGHT (default 480)"
    in
    fun () ->
      match validate_input_file in_filename, validate_dimensions width height with
      | Ok (), Ok () -> print_endline "run"
      | Error (`Msg err), _ | _, Error (`Msg err) -> eprintf "Error: %s\n" err)

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

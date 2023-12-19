[@@@warning "-69-27-33"]
open Vector

(* Module to parse the input json file as given in example_input/, into Scene.t*)

(* Given the input json file name, parse the file and pack all info about the scene into Scene.t *)
val parse_scene : string -> Scene.t

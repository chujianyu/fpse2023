open Core
(* Module to write an RGB file given lists of lists of Color.t*)

(*output using the ppm file format*)
let output_rgb_data_to_file 
~(out_filename:string )
~(width:int )
~(height:int)
(colors:Color.t list list)
=
let channel_to_int channel = 
  Float.to_int (channel *. 255.0) |> Int.max 0 |> Int.min 255 
in
let color_to_string color =
  Printf.sprintf "%d %d %d " 
  (channel_to_int @@ Color.r color) 
  (channel_to_int @@ Color.g color) 
  (channel_to_int @@ Color.b color) 
in
let lines = List.map colors 
~f:(fun row -> String.concat ~sep:"" (List.map row ~f:color_to_string)) 
in
let content = String.concat ~sep:"\n" lines in
Out_channel.with_file out_filename ~f:(fun oc ->
  Printf.fprintf oc "P3\n%d %d\n255\n%s" width height content)

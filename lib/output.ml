[@@@warning "-69-27-33-26"]
(* Module to write an RGB file given lists of rgb tuples *)
module Output = struct
(*output using the ppm file format*)
let output_rgb_data_to_file 
(file_name:string )
(width:int )
(height:int)
(rgb_list:(float*float*float) list)
  =
let output_channel = (Core.Out_channel.create file_name) in 
let header = ["P3";
(Int.to_string(width));
(Int.to_string(height));"255"] in 

let rgb_string_list = Core.List.fold_left
rgb_list
~init:[]
~f:(fun(acc)(r,g,b)-> 
  Int.to_string(Float.to_int(r*. 255.))::
  Int.to_string(Float.to_int(g*. 255.))::
  Int.to_string(Float.to_int(b*. 255.))::acc
  )
in 
let add_header = (header @ Core.List.rev(rgb_string_list)) in 
Core.Out_channel.write_all (file_name^".ppm") ~data:(Core.String.concat ~sep:"\n" add_header)



end

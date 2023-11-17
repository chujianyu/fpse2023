[@@@warning "-69-27-33"]
open Color
(* Module to write an RGB file given lists of lists of Color.t *)
module Output : 
  sig
  (* writes an .ppm file given the out_filename and 2d list of colors*)
  val output_rgb_data_to_file: out_filename:string -> width:int -> height:int -> Color.t list list -> unit
  end


[@@@warning "-69-27-33"]

module Output : 
  sig
	(*define function here*)
  val output_rgb_data_to_file: string -> int -> int -> (float*float*float) list -> unit
  end
(* Module to write an RGB file given lists of rgb tuples *)

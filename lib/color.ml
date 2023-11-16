[@@@warning "-69-27-33-32"]
open Vector
(* module Color, with r g b values b/w 0. and 1.*)
module Color = 
  struct
    type t = {
      r : float;
      g : float;
      b : float;
    }

    let make (r: float) (g: float) (b: float) = { r = r; g = g; b = b }
    let to_tuple col = (col.r, col.g, col.b)

    let to_vector color = Vector3f.create ~x:color.r ~y:color.g ~z:color.b
    let from_vector (v:Vector3f.t) = 
      let (r, g, b) = Vector3f.to_tuple v in
      make r g b 

  end
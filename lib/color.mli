[@@@warning "-69-27-33-32"]
open Vector

(* module Color, with r g b values b/w 0. and 1.*)
module Color : 
  sig
    type t [@@deriving sexp]

    val make : r:float -> g:float -> b:float -> t
    val empty : t
    val get_r : t -> float
    val get_g : t -> float
    val get_b : t -> float
    val to_tuple : t -> float * float * float

    val to_vector : t -> Vector3f.t

    val from_vector : Vector3f.t -> t

    val add : t -> t -> t
    val sub : t -> t -> t

    (* component-wise multiplication *)
    val mul : t -> t -> t
    val scale : t -> float -> t

  end
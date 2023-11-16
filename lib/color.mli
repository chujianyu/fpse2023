[@@@warning "-69-27-33-32"]
open Vector
(* module Color, with r g b values b/w 0. and 1.*)
module Color : 
  sig
    type t

    val make : float -> float -> float -> t
    val to_tuple : t -> float * float * float

    val to_vector : t -> Vector3f.t

    val from_vector : Vector3f.t -> t

  end
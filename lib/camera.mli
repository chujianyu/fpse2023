[@@@warning "-69-27-33"]
open Vector
open Ray

module Camera : sig
  (* Given indices i and j, and the width and height of the image, return the ray shot through that pixel *)
  val getRay : i:int -> j:int -> width:int -> height:int -> Ray.t
end
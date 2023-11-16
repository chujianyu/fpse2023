[@@@warning "-69-27-33"]
open Vector
open Ray

module Camera : sig
  type t 

  val create : heightAngle:float -> pos:Vector3f.t -> up:Vector3f.t -> forward:Vector3f.t -> t
  (* Given indices i and j, and the width and height of the image, return the ray shot through that pixel *)
  val getRay : i:int -> j:int -> width:int -> height:int -> Ray.t
end
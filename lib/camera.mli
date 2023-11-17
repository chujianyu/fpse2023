[@@@warning "-69-27-33-32"]
open Vector
open Ray

module Camera : sig
  type t 

  val create : height_angle:float -> pos:Vector3f.t -> up:Vector3f.t -> forward:Vector3f.t -> t
  (* Given indices i and j, and the width and height of the image, return the ray shot through that pixel 
    where (i,j) means the pixel at the i-th row, j-th column *)
  val get_ray : t -> i:int -> j:int -> width:int -> height:int -> Ray.t
  val get_pos : t -> Vector3f.t
  val get_forward : t -> Vector3f.t
  val get_up : t -> Vector3f.t
  val get_right : t -> Vector3f.t
end
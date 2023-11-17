open Vector



module Ray : sig
  type t
  val create : orig:Vector3f.t -> dir:Vector3f.t -> t
  val get_orig : t -> Vector3f.t
  val get_dir : t -> Vector3f.t
end

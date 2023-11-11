open Vector



module Ray : sig
  type t
  val create : orig:Vector3f.t -> dir:Vector3f.t -> t
end

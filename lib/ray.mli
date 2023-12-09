open Vector

(* Module that represents a Ray in the scene. *)
module Ray : sig
  type t [@@deriving sexp]
  val create : orig:Vector3f.t -> dir:Vector3f.t -> t
  val get_orig : t -> Vector3f.t
  val get_dir : t -> Vector3f.t
end

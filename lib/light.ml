open Vector
open Shape

module type L = sig
  type t [@@deriving sexp]
  val item : t
  val get_ambient : Ray.t -> Vector3f.t -> Vector3f.t -> Material.t -> Color.t
  val get_diffuse : Ray.t -> Vector3f.t -> Vector3f.t -> Material.t -> Color.t
  val get_specular : Ray.t -> Vector3f.t -> Vector3f.t -> Material.t -> Color.t
  val transparency : Vector3f.t -> (module Shape.S) list -> Color.t -> Color.t
end
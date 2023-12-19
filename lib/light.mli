open Vector
open Shape

(* Shared module type for all types of light sources. Designed to support use of first-class modules *)
module type L = sig
  type t [@@deriving sexp]
  val item : t
  val get_ambient : Ray.t -> Vector3f.t -> Vector3f.t -> Material.t -> Color.t
  val get_diffuse : Ray.t -> Vector3f.t -> Vector3f.t -> Material.t -> Color.t
  val get_specular : Ray.t -> Vector3f.t -> Vector3f.t -> Material.t -> Color.t
  (* Accumulates transparency for soft shadows under semi-transparent objects *)
  val transparency : Vector3f.t -> (module Shape.S) list -> Color.t -> Color.t
end
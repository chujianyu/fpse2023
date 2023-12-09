[@@@warning "-69-27-33"]

open Ray
open Vector
open Color
open Shape
open Sexplib.Std

module Point_light_param = struct
  type t = {pos:Vector3f.t; ambient:Color.t; diffuse:Color.t; specular:Color.t; const_atten:float; linear_atten:float; quad_atten:float}
   [@@deriving sexp]
end

module type L = sig
  type t [@@deriving sexp]
  val item : t
  val get_ambient : Ray.t -> Vector3f.t -> Vector3f.t -> Material.t -> Color.t
  val get_diffuse : Ray.t -> Vector3f.t -> Vector3f.t -> Material.t -> Color.t
  val get_specular : Ray.t -> Vector3f.t -> Vector3f.t -> Material.t -> Color.t
end
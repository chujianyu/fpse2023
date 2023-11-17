[@@@warning "-69-27-33"]

open Ray
open Vector
open Color
open Shape
open Sexplib.Std

module Point_light_param = struct
  type t = {pos:Vector3f.t; diffuse:Color.t; const_atten:float; linear_atten:float; quad_atten:float}
   [@@deriving sexp]
end

module type L = sig
  type t
  val item : t
  val get_diffuse : Ray.t -> Vector3f.t -> Vector3f.t -> Material.t -> Color.t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
end
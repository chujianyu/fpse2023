[@@@warning "-69-27-33-32"]
open Vector
open Sexplib.Std
(* module Color, with r g b values b/w 0. and 1.*)
module Color = 
  struct
    type t = {
      r : float;
      g : float;
      b : float;
    } [@@deriving sexp, fields ~getters]

    let make ~(r: float) ~(g: float) ~(b: float) = { r = r; g = g; b = b }
    let empty = make ~r:0. ~g:0. ~b:0.
    (* let get_r color = color.r
    let get_g color = color.g
    let get_b color = color.b *)

    let to_tuple col = (col.r, col.g, col.b)

    let to_vector color = Vector3f.create ~x:color.r ~y:color.g ~z:color.b
    let from_vector (v:Vector3f.t) = 
      let (r, g, b) = Vector3f.to_tuple v in
      make ~r ~g ~b 

    let add (c1: t) (c2: t) = 
      let (r1, g1, b1) = to_tuple c1 in
      let (r2, g2, b2) = to_tuple c2 in
      make ~r:(r1 +. r2) ~g:(g1 +. g2) ~b:(b1 +. b2)

    let sub (c1: t) (c2: t) = 
      let (r1, g1, b1) = to_tuple c1 in
      let (r2, g2, b2) = to_tuple c2 in
      make ~r:(r1 -. r2) ~g:(g1 -. g2) ~b:(b1 -. b2)

    let mul (c1: t) (c2: t) = 
      let (r1, g1, b1) = to_tuple c1 in
      let (r2, g2, b2) = to_tuple c2 in
      make ~r:(r1 *. r2) ~g:(g1 *. g2) ~b:(b1 *. b2)

    let scale (c: t) (s: float) = 
      let (r, g, b) = to_tuple c in
      make ~r:(r *. s) ~g:(g *. s) ~b:(b *. s)

  end
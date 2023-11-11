(* module Sphere and Shape.S may be used to create first-class modules*)
open Ray
module type S = sig
    type t
    val intersect : t -> ray:Ray.t -> float option
  end

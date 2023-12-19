open Vector

(* module Color, with r g b values b/w 0. and 1.*)

type t = {
  r : float;
  g : float;
  b : float;
} [@@deriving sexp, fields ~getters]

  val make : r:float -> g:float -> b:float -> t
  val empty : t
  
  val to_tuple : t -> float * float * float

  val to_vector : t -> Vector3f.t

  val from_vector : Vector3f.t -> t

  val add : t -> t -> t
  val sub : t -> t -> t

  (* component-wise multiplication *)
  val mul : t -> t -> t
  (* component-wise division *)
  val div : t -> t -> t
  val scale : t -> float -> t
  (* strictly greater (element-wise) *)
  val greater : t -> t -> bool


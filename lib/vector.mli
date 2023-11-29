
(* Module type for 3-dim vectors*)
module type V3 =
sig 
	type t  [@@deriving sexp]
    type elt_t  [@@deriving sexp]
	val create : x:float -> y:float -> z:float -> t
    val empty : unit ->  t
    val from_list : elt_t list -> t
    val to_tuple : t -> elt_t * elt_t * elt_t
    

    val add : t -> t -> t
    val subtract : t -> t -> t
    val scale : t -> float -> t
    val dot : t -> t -> float
    val cross : t -> t -> t
    val normalize : t -> t
    val norm : t -> float
    (*projects v1 onto v2*)
    val proj: t -> t -> t
    (*reflects v1 around v2*)
    val reflect: t -> t -> t

    val ( *: ): t -> float -> t
    val ( +: ): t -> t -> t
    val ( -: ): t -> t -> t
    val ( /: ): t -> float -> t

end

(* Module for 3-dim flaot vectors *)
module Vector3f : V3 with type elt_t = float
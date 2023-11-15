
module type V3 =
sig 
	type t
    type elt_t
	val create : x:float -> y:float -> z:float -> t
    val empty : unit ->  t
    val from_list : elt_t list -> t
    

    val add : t -> t -> t
    val subtract : t -> t -> t
    val scale : t -> float -> t
    val dot : t -> t -> float

    val ( *: ): t -> float -> t
    val ( +: ): t -> t -> t
    val ( -: ): t -> t -> t
    val ( /: ): t -> float -> t
end
(** Vectors module *)


module Vector3f : V3 with type elt_t = float
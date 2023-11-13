
module type V3 =
sig 
	type t
    type elt_t
	val create : x:float -> y:float -> z:float -> t
  val empty : unit ->  t
  val add : t -> t -> t
  val subtract : t -> t -> t
  val scale : t -> float -> t
  val dot : t -> t -> float

  val ( *: ): t -> float -> t
  val ( +: ): t -> t -> t
  val ( -: ): t -> t -> t
  val ( /: ): t -> float -> t
end

[@@@warning "-69-27"]
module Vector3f : V3 with type elt_t = float = 
struct
  type elt_t = float
  type t = { x: float; y: float; z: float }

  let create ~x ~y ~z = { x; y; z }

  let empty () = create ~x:0. ~y:0. ~z:0.

  let add v1 v2 =
    create ~x:(v1.x +. v2.x) ~y:(v1.y +. v2.y) ~z:(v1.z +. v2.z)

  let ( +: ) v1 v2 = add v1 v2

  let subtract v1 v2 =
    create ~x:(v1.x -. v2.x) ~y:(v1.y -. v2.y) ~z:(v1.z -. v2.z)

  let ( -: ) v1 v2 = subtract v1 v2

  let scale v s =
    create ~x:(v.x *. s) ~y:(v.y *. s) ~z:(v.z *. s)

  let ( *: ) v s = scale v s
  let ( /: ) v s = scale v (1. /. s)

  let dot v1 v2 =
    (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z)
  end
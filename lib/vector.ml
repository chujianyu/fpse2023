
open Core
(* Module type for 3-dim vectors*)
module type V3 =
sig 
	type t [@@deriving sexp,compare]
  type elt_t  [@@deriving sexp,compare]
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
  val proj: t -> t -> t
  
  val reflect: t -> t -> t

  val ( *: ): t -> float -> t
  val ( +: ): t -> t -> t
  val ( -: ): t -> t -> t
  val ( /: ): t -> float -> t
end

module Vector3f : V3 with type elt_t = float = struct 
  
  type elt_t = float [@@deriving sexp,compare]
  type t = { x: float; y: float; z: float } [@@deriving sexp,compare]

 

  let create ~x ~y ~z = { x; y; z }

  let empty () = create ~x:0. ~y:0. ~z:0.

  let from_list ls = match ls with
  | [x;y;z] -> create ~x ~y ~z
  | _ -> failwith "malformed list; cannot convert to Vector3f"

  let to_tuple {x;y;z} = (x,y,z)

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

  let cross v1 v2 =
    create ~x:(v1.y *. v2.z -. v1.z *. v2.y)
            ~y:(v1.z *. v2.x -. v1.x *. v2.z)
            ~z:(v1.x *. v2.y -. v1.y *. v2.x)

  let normalize v =
    let len = sqrt (dot v v) in
    if let open Float in len = 0. then v
    else scale v (1. /. len)

  let proj a b= scale b ((dot (b) (a))/. (dot (b) (b)))
  let reflect a b =  subtract (scale (proj(a)(b)) (2.)) (a) 
  let norm v = sqrt (dot v v)

end
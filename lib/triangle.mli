open Shape
(* Triagnle_params module. Represents the parameters necessary used to create a triangle*)
module Triangle_params :
  sig
    type t = 
      {
        v0 : Vertex.t;
        v1 : Vertex.t;
        v2 : Vertex.t;
        material : Material.t
      } 
      [@@deriving sexp]
  end

(* function to make a triangle module *)
val make_triangle : Triangle_params.t -> (module Shape.S)
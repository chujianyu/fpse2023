[@@@warning "-69-27-33"]
open Vector
open Ray
open Shape

let make_triangle (p : Triangle_params.t) = (module struct
 type t = Triangle_params.t  [@@deriving sexp]
 let item = p
 let intersect ~(ray:Ray.t) : Intersection_record.t option = None
end : S)
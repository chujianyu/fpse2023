[@@@warning "-69-27-33"]
open Vector
open Ray
open Shape
open Core

module Triangle_params = struct
    type t = 
      {
        v0 : Vertex.t;
        v1 : Vertex.t;
        v2 : Vertex.t;
        material : Material.t
      } 
      [@@deriving sexp]
  end

(*https://courses.cs.washington.edu/courses/csep557/14au/lectures/triangle_intersection.pdf*)
let make_triangle (p : Triangle_params.t) = (module struct
 type t = Triangle_params.t  [@@deriving sexp]
 let item = p
 let intersect ~(ray:Ray.t) : Intersection_record.t option = 
  let open Float in
  let open Vector3f in
  let edge1 = item.v1.pos -: item.v0.pos in
  let edge2 = item.v2.pos -: item.v0.pos in
  let pvec = cross (Ray.get_dir ray) edge2 in
  let det = dot edge1 pvec in

  if det > -0.00001 && det < 0.00001 then None (* if ray is parallel to surface *)
  else 
    let inv_det = 1.0 /. det in
    let tvec = (Ray.get_orig ray) -: item.v0.pos in
    let u = dot tvec pvec *. inv_det in
    if u < 0.0 || u > 1.0 then None
    else
      let qvec = cross tvec edge1 in
      let v = dot (Ray.get_dir ray) qvec *. inv_det in
      if v < 0.0 || u +. v > 1.0 then None
      else
        let t = dot edge2 qvec *. inv_det in
        if t > 0.00001 then
          let intersect_point = (Ray.get_orig ray) +: ((Ray.get_dir ray) *: t) in
          let w = 1.0 -. u -. v in
          (* using interpolated normal for smooth rendering *)
          let interpolated_normal = 
            (item.v0.normal *: w) +: 
            (item.v1.normal *: u) +: 
            (item.v2.normal *: v) |> normalize in
          Some { intersection_time = t; position = intersect_point; normal = interpolated_normal; material = item.material }
        else None

end : S)
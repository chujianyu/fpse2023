open Vector
open Shape
open Core

module Sphere_params = struct
  type t =
    {
      center : Vector3f.t;
      radius : float;
      material : Material.t
    } [@@deriving sexp]
end 

let make_sphere (p : Sphere_params.t) = (module struct
 type t = Sphere_params.t  [@@deriving sexp]
 let item = p
 let intersect ~(ray:Ray.t) : Intersection.t option= 
  let open Vector3f in
  (*(x−c)⋅(x−c)=r^2 where c is the center of sphere
     substitute x = o+td into the equation to solve for t
     where o is origin of ray, d is direction, and t is time*)
  let oc = (Ray.get_orig ray) -: p.center in
  let a = Vector3f.dot (Ray.get_dir ray) (Ray.get_dir ray) in
  let b = 2. *. (Vector3f.dot oc (Ray.get_dir ray)) in
  let c = Vector3f.dot oc oc -. (p.radius *. p.radius) in
  let discriminant = b *. b -. 4. *. a *. c in
  let open Float in
  if discriminant < 0. then None
  else
    let t1 = (-.b -. sqrt discriminant) /. (2. *. a) in
    let t2 = (-.b +. sqrt discriminant) /. (2. *. a) in
    let t = if t1 < 0. then t2 else if t2 < 0. then t1 else min t1 t2 in
    if t >= 0. then
      let position =(Ray.get_orig ray) +: ((Ray.get_dir ray) *: t) in
      let normal = (position -: p.center) |> Vector3f.normalize in
      Some {intersection_time = t; position; normal; material = item.material}
    else
      None
end : S)
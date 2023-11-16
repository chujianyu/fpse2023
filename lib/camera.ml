[@@@warning "-69-27-33"]
open Vector
open Ray

module Camera = struct

  type t = {heightAngle : float; pos : Vector3f.t; up : Vector3f.t; forward : Vector3f.t}

  let create ~heightAngle ~pos ~up ~forward = {heightAngle; pos; up; forward}

  let getRay ~i ~j ~width ~height = 
    let orig = Vector3f.empty () in
    let dir = Vector3f.empty () in
    Ray.create ~orig ~dir
end

[@@@warning "-69-27-33"]
open Vector
open Ray

module Camera = struct
  let getRay ~i ~j ~width ~height = 
    let orig = Vector3f.empty () in
    let dir = Vector3f.empty () in
    Ray.create ~orig ~dir
end

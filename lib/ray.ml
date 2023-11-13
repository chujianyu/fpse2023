[@@@warning "-69-27"]

open Vector
module Ray = struct
  type t = {
    orig : Vector3f.t;
    dir : Vector3f.t;
  }
  
  let create ~orig ~dir =
    { orig; dir }
  

end

[@@@warning "-69-27"]

open Vector
module Ray = struct
  type t = {
    orig : Vector3f.t;
    dir : Vector3f.t;
  } [@@deriving sexp]
  
  let create ~orig ~dir =
    { orig; dir }
  
  let get_orig { orig; _ } = orig
  let get_dir { dir; _ } = dir

end

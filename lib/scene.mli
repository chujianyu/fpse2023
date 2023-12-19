(*Referenced repos: 
   https://github.com/wiatrak2/raytracer
   https://github.com/CianLR/ocaml-raytracer
   https://github.com/MisakaCenter/RayTracer.ml
   *)
open Light
open Shape


(* Scene module. Represents the 3D scene in which ray tracing can be performed *)


type t = {camera:Camera.t; lights:(module L) list; shapes:(module S) list; sky_enabled:bool}

(* Converts the scene to a string representation ;
  not using sexp because "(module S) type unsupported for ppx [of_sexp] conversion" *)
val to_string : t -> string

(* Creates a scene given the necessary elements*)
val create : camera:Camera.t -> lights:(module L) list -> shapes:(module S) list -> sky_enabled:bool -> t

(* Gets the color of the ray's intersection with scene's shapes
    rLimit and cLimit are used to determine recursion stop conditions. We decided to keep them 
    as a parameter as cLimit can change (either by user input or during recursion) 
    and thus kept separate from the property of the static 3D scene. *)
val get_color : t -> Ray.t -> rLimit:int -> cLimit:Color.t -> Color.t

(* Performs ray tracing on the entire scene
   Optional argument num_domains for parallelism, listed as the first argument to avoid being unerasable *)
val ray_trace : ?num_domains:int -> t -> width:int -> height:int -> rLimit:int -> cLimit:float ->  Color.t list list

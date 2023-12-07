open Core
open OUnit2
open Fpse2023_raytracer_lib











let add_vector _ = 
  let one = Fpse2023_raytracer_lib.Vector.Vector3f.create ~x:0. ~y:1. ~z:0. in 
  let two = Fpse2023_raytracer_lib.Vector.Vector3f.create ~x:1. ~y:0. ~z:1. in 
  let expected = Fpse2023_raytracer_lib.Vector.Vector3f.create ~x:1. ~y:1. ~z:1. in
  let result = Vector.Vector3f.add one two in 
  assert_equal result @@  expected



  let reflect_vector _ = 
    let one = Fpse2023_raytracer_lib.Vector.Vector3f.create ~x:1. ~y:0. ~z:0. in 
    let two = Fpse2023_raytracer_lib.Vector.Vector3f.create ~x:0. ~y:1. ~z:0. in 
    
    let expected = Fpse2023_raytracer_lib.Vector.Vector3f.create ~x:(-1.0) ~y:0. ~z:0. in
    let result = Vector.Vector3f.reflect one two in 
    let s = (Core.Sexp.to_string(Vector.Vector3f.sexp_of_t result))  in 
    let () = Core.Printf.printf "%s" s in
    assert_equal result @@  expected

let vector_tests =
  "vector_tests"
  >: test_list
        [
      
  

          "add_vector">:: add_vector;
          "reflect_vector">:: reflect_vector;
        
          
        
        ]
let series =
  "raytracer tests"
  >::: [
    vector_tests;
  ]


  let () = run_test_tt_main series
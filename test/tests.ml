[@@@warning "-69-27-33-26-32"]
open Core
open OUnit2
open Fpse2023_raytracer_lib

(*ref:https://bheisler.github.io/post/writing-raytracer-in-rust-part-2/*)
(*ref:Ray Tracer Challenge : A Test-driven Guide to Your First 3d Renderer*)





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
    (*let s = (Core.Sexp.to_string(Vector.Vector3f.sexp_of_t result))  in 
    let () = Core.Printf.printf "%s" s in*)
    assert_equal result @@  expected






    let sphere_intersect_test _ = 
      let open Vector in
      let open Ray in 
      let open Shape in
      let open Color in
      let current_sphere_material_params:Material.t = 
      {ambient=Color.make~r:0.~g:0.~b:0.;
      specular=Color.make~r:0.~g:0.~b:0.;
      diffuse=Color.make~r:0.~g:0.~b:0.;
      emissive=Color.make~r:0.~g:0.~b:0.;
      transparent=Color.make~r:0.~g:0.~b:0.;
      shininess=0.0
      } in
      let current_sphere_params:Sphere_params.t = {center=(Vector3f.create ~x:3.~y:0.~z:0.);radius=1.;material=current_sphere_material_params} in
      let ray_one = Ray.create ~orig:(Vector3f.create ~x:0.~y:0.~z:0.) ~dir:(Vector3f.create ~x:1.~y:0.~z:0.) in 
      let sphere_one = Sphere.make_sphere current_sphere_params in 
      let module Unpacked_sphere = (val sphere_one : Shape.S) in
      let expected_pos = Vector3f.create ~x:2. ~y:0. ~z:0. in
      let result = Unpacked_sphere.intersect ~ray:ray_one in 
      match result with 
      |None -> assert false
      |Some actual_intersect_data -> (
        let () = Core.Printf.printf "%s" (Core.Sexp.to_string(Vector.Vector3f.sexp_of_t actual_intersect_data.position)) in
        assert_equal 0 @@  (Vector3f.compare expected_pos actual_intersect_data.position )
      );;
      
     

Sys_unix.chdir "../../..";;
let sphere_emission_material_no_lights_test (ctxt : test_ctxt) =
  let () = Core.Printf.printf "dir: %s \n"(Sys_unix.getcwd ()) in
(*assert true*)
assert_command 
~ctxt:ctxt
"./_build/default/bin/main.exe" 
["--in"; "example_input/emission_sphere_material_test_no_light.json"; 
"--out"; "EMISSION_TEST.ppm"; 
"--height"; "500"; 
"--width"; "500"; 
"--rLimit"; "5"; 
"--cutOff"; "0.0001"]
~foutput:(fun seq -> try 
Seq.iter (fun ch -> let() = Core.Printf.printf "%c" ch in ignore ch) seq
with End_of_file ->
())

               

let vector_tests =
  "vector_tests"
  >: test_list
        [
          "add_vector">:: add_vector;
          "reflect_vector">:: reflect_vector;
        ]

let shape_tests =
  "shape_tests"
  >: test_list
        [
          "sphere_intersect_test">:: sphere_intersect_test;
        ]
        let full_image_tests =
          "full_image_tests"
          >: test_list
                [
                  "sphere_intersect_test">:: sphere_intersect_test;
                ]
      
let series =
  "raytracer tests"
  >::: [
    vector_tests;
    shape_tests;
  ]

  let () = run_test_tt_main series
  let () =
  run_test_tt_main ("foo" >:: sphere_emission_material_no_lights_test)

  
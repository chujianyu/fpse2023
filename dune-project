(lang dune 3.11)

(name fpse2023_raytracer)

(generate_opam_files true)

(source
 (github chujianyu/fpse2023_raytracer))

(authors "Xiaoyu Guo, Amit Rehan, Chujian Yu")
(maintainers "Xiaoyu Guo, Amit Rehan, Chujian Yu")




(documentation https://url/to/documentation)

(package
 (name fpse2023_raytracer)
 (synopsis "Ocaml Functional Ray Tracer")
 (description "Ocaml Functional Ray Tracer")
 (depends 
  (ocaml (>= 5.0.0))
  (ounit2 (>= 2.2.7))
  dune
  yojson
  domainslib
  (core (>= 0.16.2))
  core_unix))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

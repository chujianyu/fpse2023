(lang dune 3.11)

(name fpse2023_raytracer)

(generate_opam_files true)

(source
 (github chujianyu/fpse2023_raytracer))

(authors "Xiaoyu Guo, Amit Rehan, Chujian Yu")
(maintainers "Xiaoyu Guo, Amit Rehan, Chujian Yu")

(package
 (name fpse2023_raytracer)
 (synopsis "Ocaml Functional Ray Tracer")
 (description "Ocaml Functional Ray Tracer, accelerated with parallelism")
 (depends 
  (ocaml (>= 5.0.0))
  (ounit2 (>= 2.2.7))
  (dune (>= 3.11.1))
  (yojson (>= 2.1.1))
  (domainslib (>= 0.5.1))
  (core (>= 0.16.2))
  (ppx_deriving (>= 5.2.1))
  (ppx_jane (>= 0.16.0))
  (bisect_ppx (>= 2.8.3))
  (core_unix (>= 0.16.0))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

# fpse2023
Function Programming in Software Engineering Course Project Fall 2023

OCaml Functional Ray Tracer

Please find our project proposal at ./fpse_project_proposal.pdf

## Usage

To build the program:
```
dune build
```

To test the binary (test output images will be placed at output/ folder):
```
dune test
```

Please note that that "dune test" requires a built binary from running "dune build" first.


Example usage:
```
./_build/default/bin/main.exe --in example_input/input1.json --out output/output_test.ppm --height 500 --width 500 --rLimit 5 --cutOff 0.0001
```


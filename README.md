# turingtape

A simulation of a single-tape turing machine using json formatted files to describe the machine.

## Dependencies

Ocaml, Ocamlc, Ocamlbuild, and Opam are required to build this project. On debian based systems the packages `ocam`l and `opam` are the only necessary installations, however this may differ from system to system. A script is provided in the setup folder for installation of these packages on a debian based system. The script will take care of opam setup as well.

## Build

In the project directory enter `make`. Enter `make native` and `make byte` for native and bytecode binaries respectively. `make` will install the yojson dependency if it does not already exist through `opam`.

## Use

`usage: ./turing [-h] jsonfile input`

`input` is a string consisting of characters described in the alphabet list in the machine file. An example of the json format that is required for a machine is included in `projdir/machines/` folder.

## Todo

* Add time complexity calculation with visualizer mode.
* Add real time graphics.
* Add support for web.

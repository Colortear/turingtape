# turingtape

A simulation of a single-tape turing machine using json formatted files to describe the machine.

## Build

In the project directory enter `make`. Enter `make native` and `make byte` for native and bytecode binaries respectively.

## Use

`usage: ./turing [-h] jsonfile input`

`input` is a string consisting of characters described in the alphabet list in the machine file. An example of the json format that is required for a machine is included in `projdir/machines/` folder.

## Todo

* Add time complexity calculation with visualizer mode.
* Add real time graphics.
* Add support for web.

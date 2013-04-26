# hackure

Tools for the hack plattform developed in "From NAND to Tetris" (nand2tetris.org) written in Clojure.

## Usage

Takes a .asm file that contains assembly code targeting the hack hardware plattform. Output is a .hack file containing the machine code from the translated assembler. The .hack file can be run in the CPU simulator for the hack plattform.

    $ java -jar hackure-0.1.0-standalone.jar my-asm-file.asm

Or just use $ lein run my-asm-file.asm

## License

Copyright © 2013 Stefan Karlsson

Distributed under the terms of the GNU General Public License Version 3

# hashlife

OCaml implementation of Bill Gosper's hashlife algorithm.
See https://en.wikipedia.org/wiki/Hashlife

Executable `gol` is a quick test program that reads a .rle file on the
command line (e.g. enclosed file `glider.rle`), and then displays and
animates the GOL using the following keys:

* `d` double the size of the macrocell
* `s` one step forward
* `r` result i.e. 2^(n-2) steps forward if the macrocell has size 2^n * 2^n
* `q` quit

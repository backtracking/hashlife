(* Hashlife (Gosper, 1984)

   Quadrants are ordered as follows:

       1 0
       2 3
*)

type cell = private {
  size: int; uid: int; ne: cell; nw: cell; sw: cell; se: cell }

val size: cell -> int
  (* a macrocell of size n is a 2^n x 2^n grid *)

val on: cell
val off: cell
  (* the two macrocells of size 0 *)

val base: int -> cell
  (* builds a 2x2 macrocell (size 1) from the 4 least significant bits
     of an integer *)

val quad: cell -> cell -> cell -> cell -> cell
  (* builds a 2^n x 2^n macrocell from 4 2^(n-1) x 2^(n-1) macrocells *)

val is_empty: cell -> bool

val empty: int -> cell
  (* an empty macrocell of size n *)

val of_matrix: bool array array -> cell
  (* [m] should be a square matrix of size [2^n x 2^n], with [n >= 2] *)

val to_matrix: cell -> bool array array

val result: cell -> cell
  (* given a macrocell of size [n], returns its result, that is
     the central macrocell of size [n-1] that is [2^(n-2)] steps in the future.
     Only meaningful for size >= 2 *)

val future: int -> cell -> cell
  (* [future s c] generalizes result to any number of steps [2^s]
     such that [s <= n-2] *)

val simplify: cell -> cell

val double: cell -> cell

val stats: Format.formatter -> unit -> unit


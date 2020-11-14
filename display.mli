
open Hashlife
open Graphics

val init: int -> unit

val clear: unit -> unit

val display_matrix: ?color:color -> bool array array -> unit

val display: ?color:color -> cell -> unit

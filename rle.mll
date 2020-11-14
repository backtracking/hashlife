
(* RLE format for the game of life *)

{
  open Format
  open Hashlife

  module Stack = struct

    type t = {
      dim : int;                 (* the final cell is 2^dim x 2^dim *)
      width : int;                                         (* 2^dim *)
      mutable st  : (int * cell array) list;           (* the stack *)
      mutable pos : int;            (* current pos on the last line *)
    }

    let print fmt st =
      fprintf fmt "@[dim = %d, width = %d, pos = %d@\n" st.dim st.width st.pos;
      List.iter (fun (d, a) -> fprintf fmt "%d/%d;@ " d (Array.length a)) st.st;
      fprintf fmt "@]"

    let create x y =
      let n, s =
        let rec find n s (* s=2^n *) =
          if s >= max x y then n, s else find (n + 1) (s * 2)
        in
        find 0 1
      in
      { dim = n; width = s; st = [0, Array.make s off]; pos = 0; }

    let merge_lines low hi =
      eprintf ".@?";
      let make i =
        let j = 2 * i in quad hi.(j + 1) hi.(j) low.(j) low.(j + 1) in
      Array.init (Array.length low / 2) make

    let rec merge st = match st.st with
      | [] -> assert false              (* always one line *)
      | (nlow, low) :: (nhi, hi) :: s when nlow = nhi ->
          assert (Array.length low = Array.length hi);
          st.st <- (nlow + 1, merge_lines low hi) :: s;
          merge st
      | _ -> ()

    let newline st =
      merge st;
      st.st <- (0, Array.make st.width off) :: st.st;
      st.pos <- 0

    let newlines st n =
      for _ = 1 to n do newline st done (* FIXME: improve *)

    let advance st n =
      st.pos <- st.pos + n;
      assert (st.pos <= st.width)

    let alive st n =
      let a = match st.st with
        | (dim, a) :: _ -> assert (dim = 0); a | _ -> assert false in
      for i = st.pos to st.pos + n - 1 do a.(i) <- on done;
      advance st n

    let dead = advance

    let rec close st =
      (* eprintf "@[close: %a@]@." print st; *)
      match st.st with
      | [] -> assert false
      | (dim, a) :: r when dim = st.dim ->
          assert (r = []); assert (Array.length a = 1); a.(0)
      | _ -> newline st; merge st; close st (* FIXME: improve *)

  end
}

let space = [' ' '\t' '\r']
let integer = ['0'-'9']+

rule header = parse
  | "#" [^ '\n']* '\n'
      { header lexbuf }
  | "x" space* "=" space* (integer as x) "," space*
    "y" space* "=" space* (integer as y) "," space*
    "rule" space* "=" space* "B3/S23" space* "\n"
      { int_of_string x, int_of_string y }

and scan m i j = parse
  | "!"
      { () }
  | eof
      { failwith "unexcepted end of file" }
  (* newlines *)
  | (integer as n) "$"
      { let n = int_of_string n in
	scan m (i + n) 0 lexbuf }
  | "$"
      { scan m (i+1) 0 lexbuf }
  | space | '\n'
      { scan m i j lexbuf }
  (* dead cells *)
  | (integer as n) "b"
      { let n = int_of_string n in
	scan m i (j + n) lexbuf }
  | "b"
      { scan m i (j + 1) lexbuf }
  (* alive cells *)
  | (integer as n) "o"
      { let n = int_of_string n in
	for k = 0 to n-1 do m.(i).(j + k) <- true done;
	scan m i (j + n) lexbuf }
  | "o"
      { m.(i).(j) <- true;
	scan m i (j + 1) lexbuf }
  | _ as c
      { Format.eprintf "unexpected character %c@." c;
	exit 1 }

and read st = parse
  | "!"
      { st }
  | eof
      { failwith "unexcepted end of file" }
  (* newlines *)
  | (integer as n) "$"
      { Stack.newlines st (int_of_string n);
	read st lexbuf }
  | "$"
      { Stack.newlines st 1;
        read st lexbuf }
  | space | '\n'
      { read st lexbuf }
  (* dead cells *)
  | (integer as n) "b"
      { Stack.dead st (int_of_string n);
	read st lexbuf }
  | "b"
      { Stack.dead st 1;
        read st lexbuf }
  (* alive cells *)
  | (integer as n) "o"
      { Stack.alive st (int_of_string n);
        read st lexbuf }
  | "o"
      { Stack.alive st 1;
	read st lexbuf }
  | _ as c
      { Format.eprintf "unexpected character %c@." c;
	exit 1 }

{
  let swap m i j =
    let tmp = m.(i) in m.(i) <- m.(j); m.(j) <- tmp

  let rev m =
    let n = Array.length m in
    for i = 0 to n/2 do swap m i (n - 1 - i) done

  let read_matrix file =
    let c = open_in file in
    let lb = Lexing.from_channel c in
    let x, y = header lb in
    let m = Array.make_matrix y x false in
    scan m 0 0 lb;
    close_in c;
    rev m;
    m

  (* a much more efficient way to read the RLE file, without using a
     boolean matrix *)

  let read_cell file =
    let c = open_in file in
    let lb = Lexing.from_channel c in
    let x, y = header lb in
    let st = read (Stack.create x y) lb in
    close_in c;
    Stack.close st

}


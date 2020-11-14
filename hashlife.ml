
(* Quadrants are ordered as follows:

       1 0
       2 3
*)

type cell = { size: int; uid: int; ne: cell; nw: cell; sw: cell; se: cell }

let rec off =
  { size = 0; uid = -2; ne = off  ; nw = off  ; sw = off  ; se = off   }
let rec on =
  { size = 0; uid = -1; ne = on   ; nw = on   ; sw = on   ; se = on    }

let size c = c.size
let unique c = c.uid

(* macrocells are hash-consed *)
module Cell = struct
  type t = cell
  let hash ({ ne = c0; nw = c1; sw = c2; se = c3; _ } as c) =
    size c + 19 * (unique c0 +
		     19 * (unique c1 +
			     19 * (unique c2 + 19 * unique c3))) land max_int
  let equal x y =
    size x = size y &&
    x.ne == y.ne && x.nw == y.nw && x.sw == y.sw && x.se == y.se
end
module H = Hashtbl.Make(Cell)

let make_cell1 i =
  let c = { size = 1; uid = i;
            ne = if i land 1 <> 0 then on else off;
            nw = if i land 2 <> 0 then on else off;
            sw = if i land 4 <> 0 then on else off;
            se = if i land 8 <> 0 then on else off } in
  c
let cells1 = Array.init 16 make_cell1
let base i = assert (0 <= i && i <= 15); cells1.(i)

let cells = H.create 5003
let () = Array.iter (fun c -> H.add cells c c) cells1 (* useful? *)

let quad =
  let u = ref 16 in
  fun c0 c1 c2 c3 ->
    let n = size c0 in
    assert (n = size c1 && n = size c2 && n = size c3);
    let c = { size = n+1; uid = !u; ne = c0; nw = c1; sw = c2; se = c3 } in
    try H.find cells c
    with Not_found -> incr u; H.add cells c c; c

module Cell1 = struct
  type t = cell
  let hash = unique
  let equal = (==)
end
module H1 = Hashtbl.Make(Cell1)

let results = H1.create 5003

let rec result c =
  try H1.find results c
  with Not_found -> let r = compute_result c in H1.add results c r; r

and compute_result c =
  let n = size c in
  assert (n >= 2);
  let c0 = c.ne and c1 = c.nw and c2 = c.sw and c3 = c.se in
  if n = 2 then
    (* game of life rule: born 3 / stay 2,3 *)
    let b c = if c == on then 1 else 0 in
    let b00 = b c0.ne and b01 = b c0.nw and b02 = b c0.sw and b03 = b c0.se in
    let b10 = b c1.ne and b11 = b c1.nw and b12 = b c1.sw and b13 = b c1.se in
    let b20 = b c2.ne and b21 = b c2.nw and b22 = b c2.sw and b23 = b c2.se in
    let b30 = b c3.ne and b31 = b c3.nw and b32 = b c3.sw and b33 = b c3.se in
    let gol c k = if k = 3 || (c == 1 && k = 2) then 1 else 0 in
    let b0 = gol b02 (b03 + b00 + b01 + b10 + b13 + b20 + b31 + b30) in
    let b1 = gol b13 (b02 + b01 + b10 + b11 + b12 + b21 + b20 + b31) in
    let b2 = gol b20 (b31 + b02 + b13 + b12 + b21 + b22 + b23 + b32) in
    let b3 = gol b31 (b30 + b03 + b02 + b13 + b20 + b23 + b32 + b33) in
    base (b0 lor (b1 lsl 1) lor (b2 lsl 2) lor (b3 lsl 3))
  else
    let { ne = _c00; nw = c01; sw = c02; se = c03; _ } = c0 in
    let { ne = c10; nw = _c11; sw = c12; se = c13; _ } = c1 in
    let { ne = c20; nw = c21; sw = _c22; se = c23; _ } = c2 in
    let { ne = c30; nw = c31; sw = c32; se = _c33; _ } = c3 in
    let r0  = result c0 in
    let r1  = result c1 in
    let r2  = result c2 in
    let r3  = result c3 in
    let r01 = result (quad c01 c10 c13 c02) in
    let r12 = result (quad c13 c12 c21 c20) in
    let r23 = result (quad c31 c20 c23 c32) in
    let r30 = result (quad c03 c02 c31 c30) in
    let rc  = result (quad c02 c13 c20 c31) in
    let a0  = result (quad r0  r01 rc  r30) in
    let a1  = result (quad r01 r1  r12 rc ) in
    let a2  = result (quad rc  r12 r2  r23) in
    let a3  = result (quad r30 rc  r23 r3 ) in
    quad a0 a1 a2 a3

(* from/to matrices *)

let is_power2 x = x > 0 && x = (x land (-x))

let of_matrix m =
  let d = Array.length m in
  if d < 2 || not (is_power2 d) then invalid_arg "of_matrix";
  Array.iter (fun a -> assert (Array.length a = d)) m;
  let rec build x y d =
    if d = 2 then
      let bit b = if b then 1 else 0 in
      let b0 = bit m.(x+1).(y+1) in
      let b1 = bit m.(x+1).(y) in
      let b2 = bit m.(x).(y) in
      let b3 = bit m.(x).(y+1) in
      base (b0 lor (b1 lsl 1) lor (b2 lsl 2) lor (b3 lsl 3))
    else
      let d = d / 2 in
      let c0 = build (x+d) (y+d) d in
      let c1 = build (x+d) y     d in
      let c2 = build x     y     d in
      let c3 = build x     (y+d) d in
      quad c0 c1 c2 c3
  in
  build 0 0 d

let to_matrix c =
  let s = 1 lsl (size c) in
  let m = Array.init s (fun _ -> Array.make s false) in
  let rec fill i j c =
    let n = size c in
    if n = 0 then begin
      if c == on then m.(i).(j) <- true
    end else
      let { ne = c0; nw = c1; sw = c2; se = c3; _ } = c in
      let d = 1 lsl (n-1) in
      fill (i+d) (j+d) c0;
      fill (i+d)  j    c1;
      fill  i     j    c2;
      fill  i    (j+d) c3
  in
  fill 0 0 c;
  m

let empty_cells = Hashtbl.create 17

let rec empty n =
  if n < 0 then invalid_arg "empty";
  try Hashtbl.find empty_cells n
  with Not_found -> let c = make_empty n in Hashtbl.add empty_cells n c; c

and make_empty n =
  if n = 0 then off
  else if n = 1 then base 0
  else let c = empty (n-1) in quad c c c c

let is_empty c = c == (empty (size c))

let double c =
  let n = size c in
  if n = 0 then
    if c == on then base 15 else base 0
  else
    let { ne = c0; nw = c1; sw = c2; se = c3; _ } = c in
    let e = empty (n-1) in
    quad (quad e e c0 e)
         (quad e e e c1)
         (quad c2 e e e)
	 (quad e c3 e e)

let rec simplify c =
  let n = size c in
  if n <= 1 then
    c
  else
    let { ne = c0; nw = c1; sw = c2; se = c3; _ } = c in
    let { ne = c00; nw = c01; sw = c02; se = c03; _ } = c0 in
    let { ne = c10; nw = c11; sw = c12; se = c13; _ } = c1 in
    let { ne = c20; nw = c21; sw = c22; se = c23; _ } = c2 in
    let { ne = c30; nw = c31; sw = c32; se = c33; _ } = c3 in
    let e = empty (n-2) in
    if c00 == e && c01 == e &&             c03 == e &&
       c10 == e && c11 == e && c12 == e &&
                   c21 == e && c22 == e && c23 == e &&
       c30 == e &&             c32 == e && c33 == e
    then
      simplify (quad c02 c13 c20 c31)
    else
      c

let futures = Hashtbl.create 17

let rec future s c =
  let h =
    try Hashtbl.find futures s
    with Not_found -> let h = H1.create 5003 in Hashtbl.add futures s h; h
  in
  try H1.find h c
  with Not_found -> let r = compute_future s c in H1.add h c r; r

and compute_future s c =
  let n = size c in
  assert (2 <= n);
  if s = n - 2 then
    result c
  else begin
    assert (0 <= s && s < n - 2);
    let { ne = c0; nw = c1; sw = c2; se = c3; _ } = c in
    let { ne = _c00; nw = c01; sw = c02; se = c03; _ } = c0 in
    let { ne = c10; nw = _c11; sw = c12; se = c13; _ } = c1 in
    let { ne = c20; nw = c21; sw = _c22; se = c23; _ } = c2 in
    let { ne = c30; nw = c31; sw = c32; se = _c33; _ } = c3 in
    let r0  = future s c0 in
    let r1  = future s c1 in
    let r2  = future s c2 in
    let r3  = future s c3 in
    let r01 = future s (quad c01 c10 c13 c02) in
    let r12 = future s (quad c13 c12 c21 c20) in
    let r23 = future s (quad c31 c20 c23 c32) in
    let r30 = future s (quad c03 c02 c31 c30) in
    let rc  = future s (quad c02 c13 c20 c31) in
    let build q0 q1 q2 q3 = quad q0.sw q1.se q2.ne q3.nw in
    let a0 = build r0  r01 rc  r30 in
    let a1 = build r01 r1  r12 rc  in
    let a2 = build rc  r12 r2  r23 in
    let a3 = build r30 rc  r23 r3  in
    quad a0 a1 a2 a3
  end

let stats fmt () =
  Format.fprintf fmt "%d macrocells@\n" (H.length cells);
  Format.fprintf fmt "%d results@\n" (H1.length results)

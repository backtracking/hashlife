
open Format
open Graphics
open Hashlife

let width = ref 512

let init w = width := w; open_graph (sprintf " %dx%d" w w)

let clear () =
  clear_graph ()

let rec draw x y w c =
  if not (is_empty c) then
    if w <= 1 then
      plot x y
    else
      let n = size c in
      if n == 0 then begin
        if c == on then fill_rect x y w w
      end else
        let w = w / 2 in
        draw (x+w) (y+w) w c.ne;
        draw x     (y+w) w c.nw;
        draw x     y     w c.sw;
        draw (x+w) y     w c.se

let display ?(color=black) c =
  set_color color;
  draw 0 0 !width c

let display_matrix ?(color=black) m =
  let s = Array.length m in
  let scale = max 1 (!width / s) in
  let r = float !width /. float s in
  set_color color;
  for i = 0 to s-1 do for j = 0 to s-1 do
    if m.(i).(j) then
      let x = truncate (float j *. r) in
      let y = truncate (float i *. r) in
      fill_rect x y scale scale
  done done

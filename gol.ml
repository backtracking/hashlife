
open Format
open Hashlife
open Graphics
open Display

let () = init 512

let file = Sys.argv.(1)
let () =
  if not (Sys.file_exists file) then begin
    eprintf "no such file: %s@." file;
    exit 1
  end

let c = Rle.read_cell file
let n = size c
let () = printf "size = %d@." n

let () = display ~color:red c

let () =
  try
    let c = ref c in
    while true do
      let st = wait_next_event [Key_pressed; Button_down] in
      if st.keypressed then begin match st.key with
	| 'q' ->
	    raise Exit
	| 'd' ->
	    c := double !c;
	    clear ();
	    display !c
	| 's' ->
	    c := future 0 !c;
	    clear ();
	    display !c
	| 'r' ->
	    let r = result !c in
	    clear ();
	    display ~color:blue r;
	    c := r
	| _ ->
	    ()
      end
    done
  with Exit ->
    close_graph ();
    printf "%a@." stats ()

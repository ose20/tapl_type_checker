open Format

module Error = struct
  
	exception Exit of int
	type info = FI of string * int * int | UNKNWON
	type 'a withinfo = {i : info; v : 'a}

	let dummyinfo = UNKNWON
	let createInfo f l c = FI (f, l, c)

	let printInfo = function
	| FI (f, l, c) ->
			printf "@[<hov 0>%s: line %d.%d@]" f l c
	| UNKNWON ->
			printf "@[<hov 0><Unknown file and line>:@]"

	(* open a box and fire f in it *)
	let errf f =
		print_flush ();
		open_hovbox 0;
		f ();
		close_box ();
		print_newline ();
		raise (Exit 1)

	let errfAt fi f =
		errf (fun () -> printInfo fi; print_space (); f ())

	let errs s =
		errf (fun () -> printf "@[<hov 0>Error: %s@]" s)
	
	let errsAt fi s =
		errfAt fi (fun () -> printf "%s" s)

	let warning s =
		printf "@[<hov 0>Warning: %s@]@." s
	
	let warningAt fi s =
		printInfo fi;
		printf "@[<hov 2>%s@]@." s

end

(* ------------------------------------------------------------------ *)

module Pervasive = struct

	let pr = print_string
	let hovbox = open_hovbox
	let vbox = open_vbox
	let cbox = close_box
	let break n m = print_break n m
	let cut = print_cut
	let space = print_space
	let newline = print_newline

end
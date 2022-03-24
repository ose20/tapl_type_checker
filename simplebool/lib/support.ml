open Format

module Error = struct
  exception Exit of int
  type info = FI of string * int * int | UNKNOWN
  type 'a withinfo = {i : info; v : 'a}

  let dummy_info = UNKNOWN
  let create_info f l c = FI(f, l, c)
  let errf f =
    open_vbox 0; f(); close_box(); print_newline(); raise (Exit 1)
  (* このライブラリの中での使う*)
  let pr = print_string

  let print_info = function
    | FI(f,l,c) -> (* f:file$(l).$(c): *)
        open_hovbox 0;
        pr f;
        pr ":";
        pr "line";
        pr @@ string_of_int l;
        pr ".";
        pr @@ string_of_int c;
        pr ": ";
        close_box ()
    | UNKNOWN ->
        open_hovbox 0;
        pr "<Unknown file and line>: ";
        close_box ()

  let errf_at fi f = errf (fun () -> print_info fi; print_space(); f())
  let errs s = errf (fun () -> pr "Error:"; pr s)
  let errs_at fi s = errf_at fi (fun () -> pr s)
  let warning s =
      pr "Warning: "; pr s; print_newline ()
  let warning_at fi s =
      print_info fi; warning s;
end


module Pervasive = struct
  let pr = Format.print_string
  let space = print_space
  let break = print_break
end
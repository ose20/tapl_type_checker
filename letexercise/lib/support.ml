open Format

module Error = struct
  exception Exit of int
  type info = FI of string * int * int | UNKNOWN
  type 'a withinfo = {i : info; v : 'a}

  let pr = print_string (*　only within this library *)
  let dummy_info = UNKNOWN
  let create_info f l c = FI (f, l, c)

  let print_info = function
    | FI (f, l, c) ->
        open_hovbox 0;
        pr f; pr ":"; pr "line"; pr @@ string_of_int l;
        pr "."; pr @@ string_of_int c; pr ": ";
        close_box()
    | UNKNOWN ->
        open_hovbox 0;
        pr "<Unknown file and line>: ";
        close_box()

  let errf f =
    open_vbox 0;
    f();
    close_box();
    raise (Exit 1)
  
  let errf_at fi f = errf (fun () -> print_info fi; print_space(); f())

  let errs s = errf (fun () -> pr "Error: "; pr s)
  let errs_at fi s = errf_at fi (fun () -> pr s)

  let warning s =
    open_hovbox 0;
    pr "Warning: "; pr s; print_newline();
    close_box()
  
  let warning_at fi s =
    open_hovbox 0;
    print_info fi; print_space();
    warning s;
    close_box()
  
end


module Pervasive = struct
  let pr = print_string
  let space = print_space
  let break = print_break
  
end
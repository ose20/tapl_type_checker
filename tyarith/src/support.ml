open Format

module Error = struct
  exception Exit of int
  type info = FI of string * int * int | UNKNOWN
  type 'a withinfo = {i : info; v : 'a}

  let dummyinfo = UNKNOWN

  let createInfo f l c = FI(f, l, c)

  let errf f =
    open_vbox 0; f(); close_box(); print_newline();
    raise (Exit 1)

  (* pr : このライブラリの中でのみ使う補助関数 *)
  let pr = print_string

  (* info 型の要素を文字列にして出力 *)
  let printInfo = function
    | FI(f,l,c) -> (* f:line($l).($c): *)
        open_hovbox 0;
        pr f;
        pr ":";
        pr "line";
        pr (string_of_int l);
        pr ".";
        pr (string_of_int c);
        pr ": ";
        close_box()
    | UNKNOWN ->
        open_hovbox 0;
        pr "<Unknown file and line>: ";
        close_box ()

  let errfAt fi f = errf(fun () -> printInfo fi; print_space(); f())
  let err s = errf (fun () -> print_string "Error: "; print_string s; print_newline())
  let error fi s = errfAt fi (fun () -> print_string s)
  let warning s = 
    print_string "Warning: "; print_string s;
    print_newline ()
  let warningAt fi s =
    printInfo fi; warning s;
end


module Pervasive = struct
  type info = Error.info
  let pr = Format.print_string
end
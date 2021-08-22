open Format

module Error = struct

  exception Exit of int
  (* error 追跡のためのファイル名と行列番号を表す型 *)
  type info = FI of string * int * int | UNKNOWN
  type 'a withinfo = {i: info; v: 'a}

  let dummyinfo = UNKNOWN
  let createInfo f l c = FI (f, l, c)


  (* エラーメッセージを出力する関数 f を受け取り  *)
  (*  それを発火してExit 1 を投げる             *)
  let errf f =
    print_flush ();
    open_hovbox 0; f ();
    close_box ();
		print_newline ();
    raise (Exit 1)

  (* info を受け取り，それを文字列にして出力 *)
  let printInfo = function
  | FI (f, l, c) ->
      printf "@[%s:%d.%d:@ @]@." f l c
  | UNKNOWN ->
      printf "@[<Unknown file and line>:@ @]@."

  (* info とエラー出力関数をもらって，それらを合成して errf に渡す *)
  let errfAt fi f =
    errf (fun () -> printInfo fi; f ())

  (* 文字列を受け取って，それからエラー出力関数を作って errf に渡す関数 *)    
  let err s = errf (fun () -> printf "Error: %s" s)

  let error fi s = errfAt fi (fun () -> printf "%s" s)

  let warning s =
    printf "Waring: %s" s;
    print_newline ()
    
  let warningAt fi s =
    printInfo fi; warning s

end

(* --------------------------------------------------------------- *)

module Pervasive = struct

  let pr = Format.print_string

end
{
open Support.Error

let reserved_words = [
  (* Keywords *)
  ("import", fun i -> Parser.IMPORT i);
  ("lam", fun i -> Parser.LAMBDA i);
  ("if", fun i -> Parser.IF i);
  ("then", fun i -> Parser.THEN i);
  ("else", fun i -> Parser.ELSE i);
  ("true", fun i -> Parser.TRUE i);
  ("false", fun i -> Parser.FALSE i);
  ("Bool", fun i -> Parser.BOOL i);

  (* Symbols *)
  ("'", fun i -> Parser.APOSTROPHE i);
  (":", fun i -> Parser.COLON i);
  (".", fun i -> Parser.DOT i);
  ("\"", fun i -> Parser.DQUOTE i);
  ("(", fun i -> Parser.LPAREN i);
  (")", fun i -> Parser.RPAREN i);
  (";", fun i -> Parser.SEMI i);
  ("_", fun i -> Parser.USCORE i);

  (* Special compound symbols *)
  ("->", fun i -> Parser.ARROW i)
]

type buildfun = info -> Parser.token
let (symbol_table : (string,buildfun) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (str,f) -> Hashtbl.add symbol_table str f) reserved_words

let createID i str = 
  try (Hashtbl.find symbol_table str) i
  with _ ->
    if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z' then
      Parser.UCID {i=i; v=str}
    else
      Parser.LCID {i=i; v=str}

let lineno    = ref 1
and depth     = ref 0
and start     = ref 0 (* offset when the last line break is read *)
and filename  = ref ""
and startlex  = ref dummy_info

let create infile stream =
  if not (Filename.is_implicit infile) then filename := infile
  else filename := Filename.concat (Sys.getcwd()) infile;
  lineno := 1; start := 0; Lexing.from_channel stream

let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)

let info lexbuf =
  create_info (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)

let text = Lexing.lexeme

let string_buffer = ref (Bytes.create 2048)
let buffer_end = ref 0

let reset_buffer () = buffer_end := 0

let add_char ch =
  let x = !buffer_end in
  let buffer = !string_buffer in
  if x = Bytes.length buffer then
    let new_buffer = Bytes.create (x*2) in
    Bytes.blit buffer 0 new_buffer 0 x;
    Bytes.set new_buffer x ch;
    string_buffer := new_buffer;
    buffer_end := x+1
  else
    Bytes.set buffer x ch;
    buffer_end := x+1

let get_str () = Bytes.sub_string (!string_buffer) 0 (!buffer_end)

}



rule main = parse
  | [' ' '\t']+
      { main lexbuf }
  | [' ' '\t']*"\n"
      { newline lexbuf; main lexbuf }
  | "*/"
      { errs_at (info lexbuf) "Unmatched end of comment" }
  | "/*"
      { depth := 1; startlex := info lexbuf; comment lexbuf; main lexbuf }
  | ['0'-'9']+
      { Parser.INTV{i=info lexbuf; v=int_of_string (text lexbuf)} }
  | ['A'-'Z' 'a'-'z' '_']
    ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
      { createID (info lexbuf) (text lexbuf) }
  | "->"
      { createID (info lexbuf) (text lexbuf) }
  | [';' '(' ')' ':' '.']
      { createID (info lexbuf) (text lexbuf) }
  | "\""
      { reset_buffer(); startlex := info lexbuf; string lexbuf }
  | eof
      { Parser.EOF(info lexbuf) }
  | _
      { errs_at (info lexbuf) "Illegal character" }

and comment = parse
  | "/*"
      { depth := succ !depth; comment lexbuf }
  | "*/"
      { depth := pred !depth; if !depth > 0 then comment lexbuf }
  | eof
      { errs_at (!startlex) "Comment not terminated" }
  | "\n"
      { newline lexbuf; comment lexbuf }
  | [^ '\n']
      { comment lexbuf }

and string = parse
  | '"' 
      { Parser.STRINGV {i = !startlex; v = get_str()} }
  | '\\'
      { add_char (escaped lexbuf); string lexbuf }
  | '\n'
      { add_char '\n'; newline lexbuf; string lexbuf }
  | eof
      { errs_at (!startlex) "String not terminated" }
  | _ 
      { add_char (Lexing.lexeme_char lexbuf 0); string lexbuf }

and escaped = parse
  | 'n' 
      { '\n' }
  | 't'
      { '\t' }
  | '\\'
      { '\\' }
  | '"'
      { '\034' }
  | '\''
      { '\'' }
  | ['0'-'9']['0'-'9']['0'-'9']
      { let x = int_of_string (text lexbuf) in
        if x > 255 then
          errs_at (info lexbuf) "Illegal character constant"
        else Char.chr x
      }
  | [^ '"' '\\' 't' 'n' '\'']
      { errs_at (info lexbuf) "Illegal character constant" }

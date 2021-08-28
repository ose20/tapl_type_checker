{
open Support.Error

let reservedWords = [
  (* keywords *)
  ("import", fun i -> Parser.IMPORT i);
  ("lam", fun i -> Parser.LAMBDA i);
  ("bind", fun i -> Parser.BIND i);
  
  (* symbols *)
  ("_", fun i -> Parser.USCORE i);
  (".", fun i -> Parser.DOT i);
  ("(", fun i -> Parser.LPAREN i);
  (")", fun i -> Parser.RPAREN i);
  (";", fun i -> Parser.SEMI i)
]

(* mutable cell *)
let lineno    = ref 1
and depth     = ref 0
and offset    = ref 0
and filename  = ref ""
and tmpinfo   = ref dummyinfo
let stringBuffer  = ref (Bytes.create 2048)
and bufferEnd     = ref 0


type buildtoken = info -> Parser.token
let (symbolTable : (string, buildtoken) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (str, f) -> Hashtbl.add symbolTable str f) reservedWords

let createID i str =
  try (Hashtbl.find symbolTable str) i
  with _ ->
    if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z'
    then Parser.UCID {i = i; v = str}
    else Parser.LCID {i = i; v = str}

let createbuf fname in_ch =
  if not (Filename.is_implicit fname) then filename := fname
  else filename := Filename.concat (Sys.getcwd ()) fname;
  lineno := 1; offset := 0; Lexing.from_channel in_ch

let proc_newline lexbuf =
  incr lineno; offset := (Lexing.lexeme_start lexbuf)

let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !offset)

let resetBuf () = bufferEnd := 0

let addChr ch =
  let pos = !bufferEnd in
  let buffer = !stringBuffer in
  if pos = Bytes.length buffer then
    begin
      let newBuffer = Bytes.create (pos * 2) in
      Bytes.blit buffer 0 newBuffer 0 pos;
      Bytes.set newBuffer pos ch;
      stringBuffer := newBuffer;
      incr bufferEnd
    end
  else
    begin
      Bytes.set buffer pos ch;
      incr bufferEnd
    end

let getStr () = Bytes.sub_string (!stringBuffer) 0 (!bufferEnd)
}

(* The main body of lexer *)
(* List of lexer: main, comment, string, escaped *)

let space = [' ' '\t']
let symbol = [';' '(' ')' '_' '.']
let digit = ['0'-'9']
let id = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '1'-'9' '\'']*

rule main = parse
  | space+
      { main lexbuf }
  | space*"\n"
      { proc_newline lexbuf; main lexbuf }
  | "*/"
      { errsAt (info lexbuf) "Unmatched end of commetn"}
  | "/*"
      { depth := 1; tmpinfo := info lexbuf; comment lexbuf; main lexbuf }
  | id
      { createID (info lexbuf) (Lexing.lexeme lexbuf) }
  | symbol
      { createID (info lexbuf) (Lexing.lexeme lexbuf) }
  | "\""
      { resetBuf (); tmpinfo := info lexbuf; string lexbuf }
  | eof
      { Parser.EOF(info lexbuf) }
  | _
      { errsAt (info lexbuf) "Illegal character" }
and comment = parse
  | "/*"
      { incr depth; comment lexbuf }
  | "*/"
      { decr depth; if !depth > 0 then comment lexbuf }
  | eof
      { errsAt (!tmpinfo) "Comment not terminated" }
  | [^ '\n']
      { comment lexbuf }
  | '\n'
      { proc_newline lexbuf; comment lexbuf }
and string = parse
  | '"'
      { Parser.STRINGV {i = !tmpinfo; v = getStr ()} }
  | '\\'
      { addChr (escaped lexbuf); string lexbuf }
  | '\n'
      { addChr '\n'; proc_newline lexbuf; string lexbuf }
  | eof
      { errsAt (!tmpinfo) "String not terminated" }
  | _
      { addChr (Lexing.lexeme_char lexbuf 0); string lexbuf }
and escaped = parse
  | 'n'
      { '\n' }
  | 't'
      { '\t' }
  | '\\'
      { '\\' }
  | '"'
      { '\"' }
  | digit digit digit
      {
        let x = int_of_string (Lexing.lexeme lexbuf) in
        if x > 255 then errsAt !tmpinfo "Illegal character constant"
        else Char.chr x
      }
  | _
      { errsAt !tmpinfo "Illegal charcter constant"}


{
open Support.Error

let reservedWords = [
  (* keywords *)
  ("import",  fun i -> Parser.IMPORT i);
  ("if",      fun i -> Parser.IF i);
  ("then",    fun i -> Parser.THEN i);
  ("else",    fun i -> Parser.ELSE i);
  ("true",    fun i -> Parser.TRUE i);
  ("false",   fun i -> Parser.FALSE i);
  ("succ",    fun i -> Parser.SUCC i);
  ("pred",    fun i -> Parser.PRED i);
  ("iszero",  fun i -> Parser.ISZERO i);
  ("Bool",    fun i -> Parser.BOOL i);
  ("Nat",     fun i -> Parser.NAT i);

  (* symbols *)
  (";",       fun i -> Parser.SEMI i);
  ("(",       fun i -> Parser.LPAREN i);
  (")",       fun i -> Parser.RPAREN i)
]

(* Support functions *)
type buildfun = info -> Parser.token
let (symbolTable : (string, buildfun) Hashtbl.t) = Hashtbl.create 1024
let _ =
  List.iter (fun (str, f) -> Hashtbl.add symbolTable str f) reservedWords

let createID i str =
  try (Hashtbl.find symbolTable str) i
  with _ ->
    if (String.get str 0) >= 'A' && (String.get str 0) <= 'Z' then
      Parser.UCID {i=i; v=str}
    else
      Parser.LCID {i=i; v=str}

let lineno    = ref 1
let depth     = ref 0
let start     = ref 0 (* 最後に改行を読んだ時の offset  *)
let filename  = ref ""
let startLex  = ref dummyinfo

(* create: string -> in_channel -> Lexing.lexbuf *)
let create inFile stream =
  if not (Filename.is_implicit inFile) then filename := inFile
  else filename := Filename.concat (Sys.getcwd()) inFile;
  lineno := 1; start := 0; Lexing.from_channel stream

let newline lexbuf = incr lineno; start := (Lexing.lexeme_start lexbuf)
(* info : Lexing.lexbuf -> Support.Error.info *)
let info lexbuf =
  createInfo (!filename) (!lineno) (Lexing.lexeme_start lexbuf - !start)

let text = Lexing.lexeme

let stringBuffer = ref (Bytes.create 2048)
(* 新しく書き込む時の場所を示すポインタ *)
let stringEnd = ref 0

let resetStr () = stringEnd := 0

(* addChar: char -> unit *)
let addChar ch =
  let x = !stringEnd in
  let buffer = !stringBuffer in
  if x = Bytes.length buffer then
    begin
      let newBuffer = Bytes.create (x*2) in
      Bytes.blit buffer 0 newBuffer 0 x;
      Bytes.set newBuffer x ch;
      stringBuffer := newBuffer;
      incr stringEnd
    end
  else
    begin
      Bytes.set buffer x ch;
      incr stringEnd
    end

let getStr () = Bytes.sub_string !stringBuffer 0 !stringEnd

}


let symbol = [';' '(' ')']

rule main = parse
  | [' ' '\t']+
      { main lexbuf }
  | "\n"
      { newline lexbuf; main lexbuf }
  | "*/"
      { error (info lexbuf) "Unmatiched end of comment" }
  | "/*"
      { depth := 1; startLex := info lexbuf; comment lexbuf; main lexbuf }
  | ['0'-'9']+
      { Parser.INTV{i = info lexbuf; v = int_of_string (text lexbuf)} }
  | ['A'-'Z' 'a'-'z' '_']
    ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']*
      { createID (info lexbuf) (text lexbuf) }
  | symbol
      { createID (info lexbuf) (text lexbuf) }
  | "\""
      { resetStr (); startLex := info lexbuf; string lexbuf }
  | eof
      { Parser.EOF(info lexbuf) }
  | _ 
      { error (info lexbuf) "Illegal character"}
and comment = parse
  | "/*"
      { incr depth; comment lexbuf }
  | "*/"
      { decr depth; if !depth > 0 then comment lexbuf }
  | eof
      { error (!startLex) "Comment not terminated"}
  | [^ '\n']
      { comment lexbuf }
  | "\n"
      { newline lexbuf; comment lexbuf }
and string = parse
  | '"'
      { Parser.STRINGV{i = !startLex; v = getStr ()} }
  | '\\'
      { addChar (escaped lexbuf); string lexbuf }
  | '\n'
      { addChar '\n'; newline lexbuf; string lexbuf }
  | eof
      { error (!startLex) "String not terminated" }
  | _
      { addChar (Lexing.lexeme_char lexbuf 0); string lexbuf }
and escaped = parse
  | 'n'
      { '\n' }
  | 't'
      { '\t' }
  | '\\'
      { '\\' }
  | '"'
      { '\034'}
  | '\''
      { '\'' }
  | ['0'-'9']['0'-'9']['0'-'9']
      { let x = int_of_string (text lexbuf) in
        if x > 255 then
          error (info lexbuf) "Illegal character constant"
        else
          Char.chr x
      }

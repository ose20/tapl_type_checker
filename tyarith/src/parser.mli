type token =
  | IMPORT of (Support.Error.info)
  | IF of (Support.Error.info)
  | THEN of (Support.Error.info)
  | ELSE of (Support.Error.info)
  | TRUE of (Support.Error.info)
  | FALSE of (Support.Error.info)
  | SUCC of (Support.Error.info)
  | PRED of (Support.Error.info)
  | ISZERO of (Support.Error.info)
  | BOOL of (Support.Error.info)
  | NAT of (Support.Error.info)
  | UCID of (string Support.Error.withinfo)
  | LCID of (string Support.Error.withinfo)
  | INTV of (int Support.Error.withinfo)
  | STRINGV of (string Support.Error.withinfo)
  | EOF of (Support.Error.info)
  | LPAREN of (Support.Error.info)
  | RPAREN of (Support.Error.info)
  | SEMI of (Support.Error.info)

val toplevel :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf ->  Syntax.command list 

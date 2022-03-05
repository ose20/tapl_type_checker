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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Support.Error
open Support.Pervasive
open Syntax
# 29 "parser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
  257 (* IMPORT *);
  258 (* IF *);
  259 (* THEN *);
  260 (* ELSE *);
  261 (* TRUE *);
  262 (* FALSE *);
  263 (* SUCC *);
  264 (* PRED *);
  265 (* ISZERO *);
  266 (* BOOL *);
  267 (* NAT *);
  268 (* UCID *);
  269 (* LCID *);
  270 (* INTV *);
  271 (* STRINGV *);
    0 (* EOF *);
  272 (* LPAREN *);
  273 (* RPAREN *);
  274 (* SEMI *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\004\000\004\000\005\000\005\000\005\000\005\000\006\000\007\000\
\007\000\007\000\000\000"

let yylen = "\002\000\
\001\000\003\000\002\000\001\000\006\000\001\000\002\000\002\000\
\002\000\001\000\001\000\001\000\001\000\003\000\001\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\011\000\012\000\000\000\000\000\
\000\000\013\000\001\000\000\000\019\000\000\000\004\000\006\000\
\010\000\003\000\000\000\007\000\008\000\009\000\000\000\000\000\
\000\000\014\000\002\000\000\000\000\000\005\000"

let yydgoto = "\002\000\
\013\000\014\000\015\000\016\000\017\000\000\000\000\000"

let yysindex = "\004\000\
\001\000\000\000\248\254\004\255\000\000\000\000\021\255\021\255\
\021\255\000\000\000\000\004\255\000\000\252\254\000\000\000\000\
\000\000\000\000\012\255\000\000\000\000\000\000\255\254\001\000\
\004\255\000\000\000\000\013\255\004\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\254\255\000\000\252\255\251\255\000\000\000\000\000\000"

let yytablesize = 273
let yytable = "\019\000\
\011\000\020\000\021\000\022\000\001\000\004\000\018\000\023\000\
\005\000\006\000\007\000\008\000\009\000\024\000\025\000\026\000\
\029\000\010\000\000\000\012\000\028\000\027\000\000\000\000\000\
\030\000\005\000\006\000\007\000\008\000\009\000\000\000\000\000\
\000\000\000\000\010\000\000\000\012\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\003\000\004\000\000\000\000\000\005\000\006\000\007\000\
\008\000\009\000\000\000\000\000\000\000\000\000\010\000\000\000\
\012\000"

let yycheck = "\004\000\
\000\000\007\000\008\000\009\000\001\000\002\001\015\001\012\000\
\005\001\006\001\007\001\008\001\009\001\018\001\003\001\017\001\
\004\001\014\001\255\255\016\001\025\000\024\000\255\255\255\255\
\029\000\005\001\006\001\007\001\008\001\009\001\255\255\255\255\
\255\255\255\255\014\001\255\255\016\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\255\255\005\001\006\001\007\001\
\008\001\009\001\255\255\255\255\255\255\255\255\014\001\255\255\
\016\001"

let yynames_const = "\
  "

let yynames_block = "\
  IMPORT\000\
  IF\000\
  THEN\000\
  ELSE\000\
  TRUE\000\
  FALSE\000\
  SUCC\000\
  PRED\000\
  ISZERO\000\
  BOOL\000\
  NAT\000\
  UCID\000\
  LCID\000\
  INTV\000\
  STRINGV\000\
  EOF\000\
  LPAREN\000\
  RPAREN\000\
  SEMI\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 39 "parser.mly"
        ( [] )
# 196 "parser.ml"
               :  Syntax.command list ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'Command) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 :  Syntax.command list ) in
    Obj.repr(
# 41 "parser.mly"
      ( let cmd = _1 in
        let cmds = _3 in
        cmd::cmds )
# 207 "parser.ml"
               :  Syntax.command list ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string Support.Error.withinfo) in
    Obj.repr(
# 46 "parser.mly"
                   ( (Import(_2.v)) )
# 215 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 47 "parser.mly"
          ( let t = _1 in Eval(tmInfo t, t) )
# 222 "parser.ml"
               : 'Command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Support.Error.info) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'Term) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'Term) in
    Obj.repr(
# 51 "parser.mly"
      ( TmIf(_1, _2, _4, _6) )
# 234 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 52 "parser.mly"
            ( _1 )
# 241 "parser.ml"
               : 'Term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 55 "parser.mly"
                 ( TmSucc(_1, _2) )
# 249 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 56 "parser.mly"
                 ( TmPred(_1, _2) )
# 257 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'AppTerm) in
    Obj.repr(
# 57 "parser.mly"
                   ( TmIsZero(_1, _2) )
# 265 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ATerm) in
    Obj.repr(
# 58 "parser.mly"
          ( _1 )
# 272 "parser.ml"
               : 'AppTerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 61 "parser.mly"
         ( TmTrue(_1) )
# 279 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 62 "parser.mly"
          ( TmFalse(_1) )
# 286 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int Support.Error.withinfo) in
    Obj.repr(
# 64 "parser.mly"
      ( let rec normalize = function
          | 0 -> TmZero(_1.i)
          | n -> TmSucc(_1.i, normalize @@ n-1)
        in normalize _1.v )
# 296 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 68 "parser.mly"
                       ( _2 )
# 305 "parser.ml"
               : 'ATerm))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'AType) in
    Obj.repr(
# 71 "parser.mly"
          ( _1 )
# 312 "parser.ml"
               : 'Type))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 74 "parser.mly"
         ( TyBool )
# 319 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 75 "parser.mly"
        ( TyNat )
# 326 "parser.ml"
               : 'AType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Support.Error.info) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'Type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Support.Error.info) in
    Obj.repr(
# 76 "parser.mly"
                       ( _2 )
# 335 "parser.ml"
               : 'AType))
(* Entry toplevel *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let toplevel (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf :  Syntax.command list )

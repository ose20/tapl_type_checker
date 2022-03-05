%{
open Support.Error
open Support.Pervasive
open Syntax
%}


/* Keyword tokens */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO
%token <Support.Error.info> BOOL
%token <Support.Error.info> NAT

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID /* uppercase-initial */
%token <string Support.Error.withinfo> LCID /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> EOF
%token <Support.Error.info> LPAREN
%token <Support.Error.info> RPAREN
%token <Support.Error.info> SEMI

%start toplevel
%type < Syntax.command list > toplevel
%%

/*  Main body of parser definition */
toplevel :
  | EOF { [] }
  | Command SEMI toplevel
      { let cmd = $1 in
        let cmds = $3 in
        cmd::cmds }

Command :
  | IMPORT STRINGV { (Import($2.v)) }
  | Term  { let t = $1 in Eval(tmInfo t, t) } 

Term :
  | IF Term THEN Term ELSE Term
      { TmIf($1, $2, $4, $6) }
  | AppTerm { $1 }

AppTerm :
  | SUCC AppTerm { TmSucc($1, $2) }
  | PRED AppTerm { TmPred($1, $2) }
  | ISZERO AppTerm { TmIsZero($1, $2) }
  | ATerm { $1 }

ATerm :
  | TRUE { TmTrue($1) }
  | FALSE { TmFalse($1) }
  | INTV // 具象構文における 0 以上の自然数のリテラルを succ ... の形に直す
      { let rec normalize = function
          | 0 -> TmZero($1.i)
          | n -> TmSucc($1.i, normalize @@ n-1)
        in normalize $1.v }
  | LPAREN Term RPAREN { $2 }

Type : 
  | AType { $1 }

AType :
  | BOOL { TyBool }
  | NAT { TyNat }
  | LPAREN Type RPAREN { $2 }
  
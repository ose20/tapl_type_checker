%{
open Support.Error
open Support.Pervasive
open Syntax
%}


/* keyword */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO

/* constant value */
%token <string Support.Error.withinfo> UCID /* uppercase-initial */
%token <string Support.Error.withinfo> LCID /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <string Support.Error.withinfo> STRINGV


/* symbolic tokens */
%token <Support.Error.info> EOF
%token <Support.Error.info> LPAREN
%token <Support.Error.info> RPAREN
%token <Support.Error.info> SEMI


/* ------------------------------------------------------------------- */

%start toplevel
%type <Syntax.command list> toplevel
%%

/* ------------------------------------------------------------------- */
/* Main body of parser difinition */
toplevel :
  | EOF
      { [] }
  | Command SEMI toplevel
      { let cmd = $1 in
        let cmds = $3 in
        cmd :: cmds }

Command :
  | IMPORT STRINGV
      { Import($2.v) }
  | Term
      { let t = $1 in Eval(info_of_tm t, t) }

Term :
  | IF Term THEN Term ELSE Term
      { TmIf($1, $2, $4, $6) }
  | AppTerm
      { $1 }

AppTerm :
  | SUCC ATerm
      { TmSucc($1, $2) }
  | PRED ATerm
      { TmPred($1, $2) }
  | ISZERO ATerm
      { TmIsZero($1, $2) }
  | ATerm
      { $1 }

ATerm :
  | TRUE
      { TmTrue($1) }
  | FALSE
      { TmFalse($1) }
  | INTV
    /*  具象構文では 0 以上の自然数リテラルも使えるようにして，
      ここで succ (...) の形に直す */
      { let rec normalize = function
        | 0 -> TmZero($1.i)
        | n -> TmSucc($1.i, normalize (n - 1))
        in normalize $1.v }
  | LPAREN Term RPAREN
			{ $2 }
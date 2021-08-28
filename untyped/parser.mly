%{
open Support.Error
open Support.Pervasive
open Syntax
%}


/* keyword */
%token <Support.Error.info> BIND
%token <Support.Error.info> IMPORT
%token <Support.Error.info> LAMBDA

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID
%token <string Support.Error.withinfo> LCID
%token <string Support.Error.withinfo> STRINGV

/* symbolic tokens */
%token <Support.Error.info> EOF
%token <Support.Error.info> DOT
%token <Support.Error.info> LPAREN
%token <Support.Error.info> RPAREN
%token <Support.Error.info> SEMI
%token <Support.Error.info> USCORE



%start toplevel
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel
%%

toplevel :
  | EOF
      { fun ctx -> ([], ctx) }
  | Command SEMI toplevel
      { fun ctx ->
          let (cmd, ctx) = $1 ctx in
          let (cmds, ctx) = $3 ctx in
          (cmd :: cmds, ctx) }

Command :
  | IMPORT STRINGV
      { fun ctx -> (Import($2.v), ctx) }
  | Term
      { fun ctx -> 
        let t = $1 ctx in (Eval(info_of_term t, t), ctx) }
  | BIND LCID
      { fun ctx -> (Bind($1, $2.v), addname ctx $2.v) }

Term :
  | AppTerm
      { $1 }
  | LAMBDA LCID DOT Term
      { fun ctx ->
          let ctx' = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx') }
  | LAMBDA USCORE DOT Term
      { fun ctx ->
          let ctx' = addname ctx "_" in
          TmAbs($1, "_", $4 ctx') }

AppTerm :
  | ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(info_of_term e1, e1, e2) }

ATerm :
  | LPAREN Term RPAREN
      { $2 }
  | LCID
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlen ctx) }

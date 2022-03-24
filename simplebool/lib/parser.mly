%{
open Support.Error
open Syntax
%}

  /* keywords */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> BOOL

%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

  /* Symbols */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> COLON
%token <Support.Error.info> DOT
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> EOF
%token <Support.Error.info> LPAREN
%token <Support.Error.info> RPAREN
%token <Support.Error.info> SEMI
%token <Support.Error.info> USCORE

  /* Special compound symbols */
%token <Support.Error.info> ARROW

%start toplevel
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel
%%

toplevel :
  | EOF
      { fun ctx -> [], ctx }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          (cmd::cmds,ctx)
      }

Command :
  | IMPORT STRINGV { fun ctx -> (Import($2.v), ctx) }
  | Term
      { fun ctx -> let t = $1 ctx in (Eval(info_of_term t, t), ctx) }
  | LCID Binder
      { fun ctx -> (Bind($1.i, $1.v, $2 ctx), add_name ctx $1.v) }

Binder :
  | COLON Type { fun ctx -> VarBind ($2 ctx) }

Type :
  | ArrowType { $1 }

ArrowType :
  | AType ARROW ArrowType { fun ctx -> TyArr($1 ctx, $3 ctx) }
  | AType { $1 }

AType :
  | BOOL { fun _ -> TyBool }
  | LPAREN Type RPAREN { $2 }


Term :
  | AppTerm { $1 }
  | LAMBDA LCID COLON Type DOT Term
      { fun ctx ->
          let ctx1 = add_name ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1)
      }
  | LAMBDA USCORE COLON Type DOT Term
      { fun ctx ->
          let ctx1 = add_name ctx "_" in
          TmAbs($1, "_", $4 ctx, $6 ctx1) }
  | IF Term THEN Term ELSE Term
      { fun ctx -> TmIf($1, $2 ctx, $4 ctx, $6 ctx) }

AppTerm :
  | ATerm { $1 }
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(info_of_term e1, e1, e2) }

ATerm :
  | LPAREN Term RPAREN { $2 }
  | LCID
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctx_len ctx) }
  | TRUE { fun _ -> TmTrue($1) }
  | FALSE { fun _ -> TmFalse($1) }
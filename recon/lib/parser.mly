%{
open Support.Error
open Syntax
%}

/* ---- Keywords ---- */
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> IF
%token <Support.Error.info> THEN
%token <Support.Error.info> ELSE
%token <Support.Error.info> TRUE
%token <Support.Error.info> FALSE
%token <Support.Error.info> BOOL
%token <Support.Error.info> SUCC
%token <Support.Error.info> PRED
%token <Support.Error.info> ISZERO
%token <Support.Error.info> NAT
%token <Support.Error.info> LET
%token <Support.Error.info> IN

/* ---- Identifier and constant value tokens */
%token <string Support.Error.with_info> UCID     /* uppercase-initial */
%token <string Support.Error.with_info> LCID     /* lowercase/symbolic-initial */
%token <int Support.Error.with_info> INTV
%token <string Support.Error.with_info> STRINGV

/* ----- Symbols ----- */
%token <Support.Error.info> ARROW
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> COLON
%token <Support.Error.info> DOT
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> DSEMI
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> LPAREN
%token <Support.Error.info> RPAREN
%token <Support.Error.info> SEMI
%token <Support.Error.info> USCORE


%start toplevel
%type <Syntax.context -> (Syntax.command list * Syntax.context)> toplevel
%%

toplevel:
  | EOF
      { fun ctx -> ([], ctx) }
  | Command DSEMI toplevel
      { fun ctx ->
          let cmd, ctx = $1 ctx in
          let cmds, ctx = $3 ctx in
          (cmd::cmds, ctx) }

// Syntax.context -> (Syntax.Command, Syntax.context) 
Command:
  | Term
      { fun ctx -> let t = $1 ctx in (Eval(info_of_term t, t), ctx) }

// Syntax.ty
Type:
  | ArrowType { $1 }

ArrowType:
  | AType ARROW ArrowType
      { Ty_arr($1, $3) }
  | AType
      { $1 }

AType:
  | LPAREN Type RPAREN { $2 }
  | UCID
      { Ty_id($1.v) }
  | BOOL
      { Ty_bool }
  | NAT
      { Ty_nat }


// Syntax.context -> Syntax.term 
Term:
  | AppTerm { $1 }
  | LAMBDA LCID COLON Type DOT Term
      { fun ctx ->
          let ctx1 = add_name ctx $2.v in 
          Tm_abs($1, $2.v, Some $4, $6 ctx1) }
  | LAMBDA USCORE COLON Type DOT Term
      { fun ctx ->
          let ctx1 = add_name ctx "_" in
          Tm_abs($1, "_", Some $4, $6 ctx1) }
  | LAMBDA LCID DOT Term
      { fun ctx ->
          let ctx' = add_name ctx $2.v in
          Tm_abs ($1, $2.v, None, $4 ctx') }
  | IF Term THEN Term ELSE Term
      { fun ctx -> Tm_if($1, $2 ctx, $4 ctx, $6 ctx) }
  | LET LCID EQ Term IN Term
      { fun ctx ->
          let ctx' = add_name ctx $2.v in
          Tm_let($1, $2.v, $4 ctx, $6 ctx') }
  | LET USCORE EQ Term IN Term
      { fun ctx ->
          let ctx' = add_name ctx "_" in
          Tm_let ($1, "_", $4 ctx, $6 ctx')}

AppTerm:
  | ATerm { $1 }
  | AppTerm ATerm
      { fun ctx -> 
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          Tm_app(info_of_term e1, e1, e2)}
  | SUCC ATerm
      { fun ctx ->
          Tm_succ($1, $2 ctx) }
  | PRED ATerm
      { fun ctx -> 
          Tm_pred($1, $2 ctx) }
  | ISZERO ATerm
      { fun ctx ->
          Tm_iszero($1, $2 ctx) }

ATerm:
  | LPAREN Term RPAREN 
      { $2 }
  | LCID
      { fun ctx ->
          Tm_var($1.i, name2index $1.i ctx $1.v, ctx_len ctx) }
  | TRUE
      { fun _ -> Tm_true($1) }
  | FALSE
      { fun _ -> Tm_false($1) }
  | INTV
      { fun _ ->
          let rec f n = match n with
            | 0 -> Tm_zero($1.i)
            | n -> Tm_succ($1.i, f (n-1))
          in f $1.v }

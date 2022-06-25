open Recon
open Format
open Support.Error
open Support.Pervasive
open Syntax
open Core

let search_path = ref [""]

let arg_defs = [
  "-I",
  Arg.String (fun dir -> search_path := dir :: !search_path),
  "Append a directory to the search path"
]

let open_file file =
  let rec try_next = function
    | [] -> errs ("Could not find " ^ file)
    | dir :: rest ->
        let name = if dir = "" then file else (dir ^ "/" ^ file) in
        try open_in name
        with Sys_error _ -> try_next rest
  in try_next !search_path

let parse_file file =
  let in_ch = open_file file in
  let lexbuf = Lexer.create file in_ch in
  let result =
    try 
      Parser.toplevel Lexer.main lexbuf 
    with
      | Parsing.Parse_error ->
          errs_at (Lexer.info lexbuf) "Parse error"
  in Parsing.clear_parser (); close_in in_ch; result

let process_command (next_uvar, constr) = function
  | Eval (fi, t) ->
      let (ty_t, next_uvar', constr_t) = recon empty_context next_uvar t in
      let t' = eval empty_context t in
      let constr' = combine_constr constr constr_t in
      let type_subst =
        unify fi "Could not simplify constraints" constr' in
      printtm empty_context t';
      break 1 2;
      pr ": ";
      open_hovbox 0;
      printty (apply_subst type_subst ty_t);
      print_newline ();
      close_box ();
      (next_uvar', type_subst)

let process_file file (next_uvar, constr) =
  let cmds, _ = parse_file file empty_context in
  let g (next_uvar, constr) cmd =
    open_hvbox 0;
    let results = process_command (next_uvar, constr) cmd in
    print_flush ();
    results
  in
  List.fold_left g (next_uvar, constr) cmds


let parse_args () =
  let in_file = ref (None : string option) in
  Arg.parse arg_defs
    (fun s ->
      match !in_file with
      | Some _ -> errs "You must specify exactry one input file"
      | None -> in_file := Some s)
    "";
  match !in_file with
  | None -> errs "You must specify an input file"
  | Some s -> s

let main () =
  let in_file = parse_args () in
  let _ = process_file in_file (uvar_gen, empty_constr) 
  in ()

let () =
  main ();
  print_flush ();
  exit 0



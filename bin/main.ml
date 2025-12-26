let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let ast = Lambda.Parser.prog Lambda.Lexer.token lexbuf in
    close_in ic;
    ast
  with
  | Lambda.Lexer.SyntaxError msg ->
      close_in ic;
      Printf.eprintf "Syntax error: %s\n" msg;
      exit 1
  | Lambda.Parser.Error ->
      close_in ic;
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Parse error at line %d, column %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1


let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s [--lazy] <file>\n" Sys.argv.(0);
    exit 1
  end;
  
  let use_lazy = ref false in
  let filename = ref "" in

  for i = 1 to Array.length Sys.argv - 1 do
    match Sys.argv.(i) with
    | "--lazy" -> use_lazy := true
    | arg -> filename := arg
  done;
  
  if !filename = "" then begin
    Printf.eprintf "Usage: %s [--lazy] <file>\n" Sys.argv.(0);
    exit 1
  end;
  
  let ast = parse_file !filename in

  match Lambda.Checker.infer ast with 
  | Error msg -> Printf.printf "Type error: %s\n" msg; exit 1;
  | Ok _ -> ();  
  
  if !use_lazy then begin
    let result = Lambda.Eval.lazyEval [] ast in
    Printf.printf "Result (lazy): %s \n" 
      (Lambda.Eval.string_of_cbn_result result)
  end else begin
    let result = Lambda.Eval.strictEval [] ast in
    Printf.printf "Result (strict): %s \n" 
      (Lambda.Eval.string_of_cbv_result result)
  end

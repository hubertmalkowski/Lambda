
let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let ast = Lambda_let_poly.Parser.prog Lambda_let_poly.Lexer.token lexbuf in
    close_in ic;
    ast
  with
  | Lambda_let_poly.Lexer.SyntaxError msg ->
      close_in ic;
      Printf.eprintf "Syntax error: %s\n" msg;
      exit 1
  | Lambda_let_poly.Parser.Error ->
      close_in ic;
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Parse error at line %d, column %d\n"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      exit 1


let repl () = 
  let eval code = 
      let ast = Lambda_let_poly.Parser.prog Lambda_let_poly.Lexer.token (Lexing.from_string code) in
      match Lambda_let_poly.Infer.typecheck ast with 
        | Error msg -> Printf.sprintf "Type error: %s\n" msg
        | Ok t -> 
          let result = Lambda_let_poly.Eval.strictEval [] ast in
          (match result with
            | Ok v -> (Lambda_let_poly.Eval.string_of_cbv v) ^ " : " ^  Lambda_let_poly.Infer.string_of_typ t ^ "\n"
            | Error _ -> "Runtime error: ")
    in
    Lambda_utils.Utils.repl eval
      

      
      
      
      


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
    | "repl" -> repl ()
    | arg -> filename := arg
  done;
  
  if !filename = "" then begin
    Printf.eprintf "Usage: %s [--lazy] <file>\n" Sys.argv.(0);
    exit 1
  end;
  
  let ast = parse_file !filename in

  match Lambda_let_poly.Infer.typecheck ast with 
  | Error msg -> Printf.printf "Type error: %s\n" msg; exit 1;
  | Ok t -> Printf.printf "%s" (Lambda_let_poly.Infer.string_of_typ t);  
  
  if !use_lazy then begin
    let result = Lambda_let_poly.Eval.lazyEval [] ast in
    Printf.printf " (lazy)\n%s \n" 
      (Lambda_let_poly.Eval.string_of_cbn_result result)
  end else begin
    let result = Lambda_let_poly.Eval.strictEval [] ast in
    Printf.printf "\n%s \n" 
      (Lambda_let_poly.Eval.string_of_cbv_result result)
  end

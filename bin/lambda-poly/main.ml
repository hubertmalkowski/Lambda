open Lambda_poly

let format_parse_error ?message ~code ~line ~column ~error_type () =
  let lines = String.split_on_char '\n' code in
  (* Build the header *)
  let header =
    match message with
    | Some msg ->
        Printf.sprintf "%s at line %d, column %d: %s" error_type line column msg
    | None -> Printf.sprintf "%s at line %d, column %d" error_type line column
  in
  (* Try to get the error line and create pointer *)
  match List.nth_opt lines (line - 1) with
  | Some error_line ->
      let pointer = String.make column ' ' ^ "^" in
      Printf.sprintf "%s\n  %s\n  %s" header error_line pointer
  | None ->
      (* Line out of bounds, just return header *)
      header

let repl () =
  let env : Infer.ctx ref =
    ref Infer.{ vars = []; exceptions = []; typs = [] }
  in
  let eval code =
    let lexbuf = Lexing.from_string code in
    try
      let ast = Parser.decl Lexer.token lexbuf in
      (Infer.infer_decl !env ast |> Infer.inferred_to_repl env) ^ "\n"
    with
    | Lambda_poly.Lexer.SyntaxError msg ->
        let pos = lexbuf.lex_curr_p in
        format_parse_error ~message:msg ~code ~line:pos.pos_lnum
          ~column:(pos.pos_cnum - pos.pos_bol)
          ~error_type:"Syntax error" ()
    | Lambda_poly.Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        format_parse_error ~code ~line:pos.pos_lnum
          ~column:(pos.pos_cnum - pos.pos_bol)
          ~error_type:"Parse error" ()
  in
  Lambda_utils.Utils.repl eval

let () = repl ()

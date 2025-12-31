let repl () =
  let ctx : (string * Lambda_system_f.Typecheck.typ) list ref = ref [] in
  let eval code =
    let lexbuf = Lexing.from_string code in
    try
      let ast =
        Lambda_system_f.Parser.prog Lambda_system_f.Lexer.token lexbuf
      in
      match Lambda_system_f.Typecheck.typecheck_program !ctx ast with
      | Ok (tresult, ctx') ->
          ctx := ctx';
          Lambda_system_f.Typecheck.string_of_typ tresult
      | Error e -> "Type error: " ^ e
    with
    | Lambda_system_f.Lexer.SyntaxError msg -> "Syntax error: " ^ msg
    | Lambda_system_f.Parser.Error ->
        let pos = lexbuf.lex_curr_p in
        let line = pos.pos_lnum in
        let col = pos.pos_cnum - pos.pos_bol in
        Printf.sprintf "Parse error at line %d, column %d" line col
  in
  Lambda_utils.Utils.repl eval

let () = repl ()

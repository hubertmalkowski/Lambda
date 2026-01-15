let format_parse_error ?message ~code ~line ~column ~error_type () =
  let lines = String.split_on_char '\n' code in
  (* Build the header *)
  let header =
    match message with
    | Some msg ->
        Printf.sprintf "%s at line %d, column %d: %s\n" error_type line column
          msg
    | None -> Printf.sprintf "%s at line %d, column %d\n" error_type line column
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
  let tctx : (string * Lambda_system_f.Typecheck.typ) list ref = ref [] in
  let vctx : (string * Lambda_system_f.Eval.value) list ref = ref [] in
  let eval code =
    let lexbuf = Lexing.from_string code in
    try
      let ast =
        Lambda_system_f.Parser.prog Lambda_system_f.Lexer.token lexbuf
      in
      match Lambda_system_f.Typecheck.typecheckRepl !tctx ast with
      | Ok (tresult, ctx') -> (
          tctx := ctx';
          let tresult = Lambda_system_f.Typecheck.string_of_typ tresult in
          match Lambda_system_f.Eval.interpretProgram !vctx ast with
          | Ok (res, ctx) ->
              vctx := ctx;
              res ^ " : " ^ tresult
          | Error ((start, _), msg) ->
              format_parse_error ~message:msg ~line:start.pos_lnum ~code
                ~column:(start.pos_cnum - start.pos_bol)
                ~error_type:"Runtime error" ())
      | Error ((start, _), msg) ->
          format_parse_error ~message:msg ~line:start.pos_lnum ~code
            ~column:(start.pos_cnum - start.pos_bol)
            ~error_type:"Type error" ()
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

let repl () =
  let ctx : (string * Lambda_system_f.Typecheck.typ) list ref = ref [] in
  let eval code =
    let ast =
      Lambda_system_f.Parser.prog Lambda_system_f.Lexer.token
        (Lexing.from_string code)
    in
    match Lambda_system_f.Typecheck.typecheck_program !ctx ast with
    | Ok (tresult, ctx') ->
        ctx := ctx';
        Lambda_system_f.Typecheck.string_of_typ tresult
    | Error e -> "Type error: " ^ e
  in
  Lambda_utils.Utils.repl eval

let () = repl ()

%{
   open Syntax

   let param_fold (n, t) acc = Lambda (n, t, acc)
%}

%token <string> ID
%token <int> INT
%token LPAREN RPAREN
%token FUN ARROW EQUAL LET IN SUCC PRED COLON
%token TRUE FALSE ISZERO IF THEN ELSE
%token RAISE EXCEPTION TYPE
%token TRY WITH PIPE
%token EOF

%right ARROW

%start <Syntax.decl> decl

%%


decl:
        | e = expr EOF { PExpr e }
        | LET x = ID params = list(param) EQUAL e1 = expr EOF
            { let fn = List.fold_right param_fold params e1 in PDef (x, fn) }
        | TYPE n = ID EQUAL tt = typ_signature EOF { PTypeDef (n, tt) }
        | EXCEPTION n = ID EQUAL tt = typ_signature EOF
            { PExceptionDef (n, Some tt) }
        | EXCEPTION n = ID EOF
            { PExceptionDef (n, None) }
;


typ_signature:
    | a = typ_signature ARROW b = typ_signature
        { TSArrow (a, b) }
    | a = ID
        { TS a }
    | LPAREN tt = typ_signature RPAREN
        { tt }
;


expr:
  | simple_expr { $1 }
  | app_expr { $1 }
  | FUN params = nonempty_list(param) ARROW body = expr
      { List.fold_right param_fold params body }
  | LET x = ID params = list(param) EQUAL e1 = expr IN e2 = expr
      { let fn = List.fold_right param_fold params e1 in
        Let (x, fn, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
      { If (e1, e2, e3) }
  | SUCC e = simple_expr
      { Succ e }
  | PRED e = simple_expr
      { Pred e }
  | ISZERO e = simple_expr
      { IsZero e }
  | RAISE id = ID LPAREN e = expr RPAREN  { Raise (id, Some e) }
  | RAISE id = ID { Raise (id, None) }
  | TRY e = expr WITH cases = catch_cases
      { TryCatch (e, cases) }
;

catch_cases:
  | PIPE? c = separated_nonempty_list(PIPE, catch_case) { c }
;

catch_case:
  | p = catch_pattern ARROW e = expr { (p, e) }
;

catch_pattern:
  | ex_name = ID x = ID { (ex_name, Some x) }
  | ex_name = ID { (ex_name, None) }
;

param:
  | LPAREN x = ID COLON t = typ_signature RPAREN
      { (x, Some t) }
  | x = ID
      { (x, None) }
  ;


app_expr:
  | simple_expr simple_expr
      { App ($1, $2) }
  | app_expr simple_expr
      { App ($1, $2) }
;


simple_expr:
  | x = ID { Var x }
  | n = INT { Int n }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LPAREN e = expr RPAREN { e }
;

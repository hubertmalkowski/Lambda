%{
open Ast
%}

%token <string> ID
%token <int> INT
%token LPAREN RPAREN LBRACE RBRACE COMMA DOT
%token FUN ARROW EQUAL LET IN SUCC PRED COLON
%token TRUE FALSE ISZERO IF THEN ELSE
%token EOF

%start <Ast.expr> prog

%%

prog:
  | e = expr EOF { e }
  ;

expr:
  | simple_expr { $1 }
  | app_expr { $1 }
  | FUN params = nonempty_list(param) ARROW body = expr
      { List.fold_right (fun (x, t) acc -> Lambda (x, t, acc)) params body }
  | LET x = ID params = list(param) EQUAL e1 = expr IN e2 = expr
      { let fn = List.fold_right (fun (p, t) acc -> Lambda (p, t, acc)) params e1 in
        Let (x, fn, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
      { If (e1, e2, e3) }
  | SUCC e = simple_expr
      { Succ e }
  | PRED e = simple_expr
      { Pred e }
  | ISZERO e = simple_expr
      { IsZero e }
  ;

param:
  | LPAREN x = ID COLON t = typ_signature RPAREN
      { (x, Some t) }
  | x = ID
      { (x, None) }
  ;

typ_signature:
    | LPAREN a = typ_signature ARROW b = typ_signature RPAREN
        { TSArrow (a, b) }
    | LBRACE sigs = separated_nonempty_list(COMMA, typ_signature) RBRACE
      { TSTuple sigs }
    | a = ID
        { TS a }
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
  | simple_expr DOT n = INT
      { TupleProj ($1, n) }
  ;

%{
open Ast
%}

%token <string> ID
%token <int> INT
%token LPAREN RPAREN
%token FUN ARROW EQUAL LET IN SUCC PRED
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
  | FUN x = ID ARROW body = expr
      { Lambda (x, body) }
  | LET x = ID EQUAL e1 = expr IN e2 = expr
      { Let (x, e1, e2) }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr
      { If (e1, e2, e3) }
  | SUCC e = simple_expr
      { Succ e }
  | PRED e = simple_expr
      { Pred e }
  | ISZERO e = simple_expr
      { IsZero e }
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

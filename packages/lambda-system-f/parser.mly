%{
   open Syntax


   type param_helper =
                | TypeParam of string
                | Param of string * typ_sig

   let param_fold p acc = match p with
                | Param (n, t) -> Lambda (n, t, acc)
                | TypeParam n -> TypeLambda (n, acc)


%}


%token <string> ID
%token <int> INT
%token LPAREN RPAREN LBRACKET RBRACKET
%token FUN ARROW EQUAL LET IN SUCC PRED COLON
%token TRUE FALSE ISZERO IF THEN ELSE
%token EOF

%right ARROW

%start <Syntax.program> prog

%%

prog:
        | e = expr EOF { PExpr e }
        | LET d = def EOF { PDef d }
;

def: 
        | x = ID params = list(param) EQUAL e1 = expr
            { let fn = List.fold_right param_fold params e1 in (x, fn) }
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
;


param:
  | LBRACKET x = ID RBRACKET
      { TypeParam x }
  | LPAREN x = ID COLON t = typ_signature RPAREN
      { Param (x, t) }
;



typ_signature:
    | a = typ_signature ARROW b = typ_signature
        { TSArrow (a, b) }
    | a = ID
        { TS a }
    | LPAREN tt = typ_signature RPAREN
        { tt }
;


app_expr:
  | simple_expr LBRACKET typ_signature RBRACKET
      { TApp ($1, $3) }
  | app_expr LBRACKET typ_signature RBRACKET
      { TApp ($1, $3) }
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

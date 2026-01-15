%{
   open Syntax

   type param_helper =
                | TypeParam of string
                | Param of string * typ_sig

   let param_fold p acc = (match p with
                | Param (n, t) -> { item = Lambda (n, t, acc); loc = acc.loc }
                | TypeParam n -> { item = TypeLambda (n, acc); loc = acc.loc })

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

%inline located(X):
  | x = X { { item = x; loc = $loc } }

prog:
        | e = loc_expr EOF { PExpr e }
        | LET d = def EOF { PDef d }
;

def: 
        | x = ID params = list(param) EQUAL e1 = loc_expr
            { let fn = List.fold_right param_fold params e1 in (x, fn) }
;


loc_expr:
  | e = located(expr) { e }

expr:
  | simple_expr { $1 }
  | app_expr { $1 }
  | FUN params = nonempty_list(param) ARROW body = loc_expr
      { (List.fold_right param_fold params (body)).item }
  | LET x = ID params = list(param) EQUAL e1 = loc_expr IN e2 = loc_expr
      { let fn = List.fold_right param_fold params (e1) in
        Let (x, fn, e2) }
  | IF e1 = loc_expr THEN e2 = loc_expr ELSE e3 = loc_expr
      { If (e1, e2, e3) }
  | SUCC e = loc_simple_expr
      { Succ (e) }
  | PRED e = loc_simple_expr
      { Pred (e) }
  | ISZERO e = loc_simple_expr
      { IsZero (e) }
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
  | e = simple_expr LBRACKET t = typ_signature RBRACKET
      { TApp ({ item = e; loc = $loc(e) }, t) }
  | e = app_expr LBRACKET t = typ_signature RBRACKET
      { TApp ({ item = e; loc = $loc(e) }, t) }
  | e1 = simple_expr e2 = simple_expr
      { App ({ item = e1; loc = $loc(e1) }, { item = e2; loc = $loc(e2) }) }
  | e1 = app_expr e2 = simple_expr
      { App ({ item = e1; loc = $loc(e1) }, { item = e2; loc = $loc(e2) }) }
;

loc_simple_expr:
  | e = located(simple_expr) { e }
    

simple_expr:
  | x = ID { Var x }
  | n = INT { Int n }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LPAREN e = loc_expr RPAREN { e.item }
;

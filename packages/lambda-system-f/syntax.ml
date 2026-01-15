type typ_sig = TSArrow of typ_sig * typ_sig | TS of string
type 'a located = { item : 'a; loc : Lexing.position * Lexing.position }

type expr =
  | Var of string
  | Lambda of string * typ_sig * located_expr
  | TypeLambda of string * located_expr
  | App of located_expr * located_expr
  | TApp of located_expr * typ_sig
  | Let of string * located_expr * located_expr
  | Int of int
  | Bool of bool
  | Succ of located_expr
  | Pred of located_expr
  | IsZero of located_expr
  | If of located_expr * located_expr * located_expr

and located_expr = expr located
and program = PExpr of located_expr | PDef of (string * located_expr)

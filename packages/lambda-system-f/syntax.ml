type typ_sig = TSArrow of typ_sig * typ_sig | TS of string

type expr =
  | Var of string
  | Lambda of string * typ_sig * expr
  | TypeLambda of string * expr
  | App of expr * expr
  | TApp of expr * typ_sig
  | Let of string * expr * expr
  | Int of int
  | Bool of bool
  | Succ of expr
  | Pred of expr
  | IsZero of expr
  | If of expr * expr * expr

type program = PExpr of expr | PDef of (string * expr)

type typ_sig = TSArrow of typ_sig * typ_sig | TS of string

type expr =
  | Var of string
  | Lambda of string * typ_sig option * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Int of int
  | Bool of bool
  | Succ of expr
  | Pred of expr
  | IsZero of expr
  | If of expr * expr * expr
  | Raise of string * expr option
  | TryCatch of expr * (pattern * expr) list

and pattern = string * string option

type decl =
  | PExpr of expr
  | PDef of (string * expr)
  | PExceptionDef of (string * typ_sig option)
  (* (name, params, sig) *)
  | PTypeDef of (string * typ_sig)

type program = decl list

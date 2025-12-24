type typ_sig = 
    | TSArrow of typ_sig * typ_sig
    | TS of string

type expr =
  | Var of string
  | Lambda of string * typ_sig * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Int of int
  | Bool of bool
  | Succ of expr
  | Pred of expr
  | IsZero of expr
  | If of expr * expr * expr

let rec string_of_expr = function
  | Var x -> x
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Lambda (x, _, body) ->
      Printf.sprintf "fun %s -> %s" x (string_of_expr body)
  | Succ e ->
      Printf.sprintf "succ %s" (string_of_simple e)
  | Pred e ->
      Printf.sprintf "pred %s" (string_of_simple e)
  | IsZero e ->
      Printf.sprintf "iszero %s" (string_of_simple e)
  | If (e1, e2, e3) ->
      Printf.sprintf "if %s then %s else %s" 
        (string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | App (f, arg) ->
      Printf.sprintf "%s %s" (string_of_app f) (string_of_simple arg)
  | Let (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x (string_of_expr e1) (string_of_expr e2)

and string_of_simple = function
  | Var _ | Int _ | Bool _ as e -> string_of_expr e
  | e -> Printf.sprintf "(%s)" (string_of_expr e)

and string_of_app = function
  | App _ as e -> string_of_expr e
  | Var _ | Int _ | Bool _ as e -> string_of_expr e
  | e -> Printf.sprintf "(%s)" (string_of_expr e)

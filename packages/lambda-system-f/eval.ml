open Syntax
open Lambda_utils.Utils

type value =
  | VArrow of string * located_expr
  | VInt of int
  | VTypeLambda of located_expr
  | VBool of bool

let string_of_value = function
  | VArrow (_, _) -> ""
  | VInt i -> string_of_int i
  | VBool i -> string_of_bool i
  | VTypeLambda _ -> ""

type ctx = (string * value) list

let rec interpret_expr context expr =
  match expr.item with
  | Var name -> (
      match List.assoc_opt name context with
      | Some v -> Ok v
      | None -> Error (expr.loc, "Variable " ^ name ^ " undefined"))
  | Lambda (param, _, body) -> Ok (VArrow (param, body))
  | TypeLambda (_, body) -> Ok (VTypeLambda body)
  | App (lambda, param) -> (
      let* elambda = interpret_expr context lambda in
      match elambda with
      | VArrow (paramname, body) ->
          let* eparam = interpret_expr context param in
          interpret_expr ((paramname, eparam) :: context) body
      | _ -> Error (lambda.loc, "Not a function"))
  | TApp (e, _) -> (
      let* elambda = interpret_expr context e in
      match elambda with
      | VTypeLambda body -> interpret_expr context body
      | _ -> Error (e.loc, "Not a type abs"))
  | Let (name, value, body) ->
      let* vvalue = interpret_expr context value in
      interpret_expr ((name, vvalue) :: context) body
  | Int a -> Ok (VInt a)
  | Bool a -> Ok (VBool a)
  | Succ a -> (
      let* va = interpret_expr context a in
      match va with VInt i -> Ok (VInt (i + 1)) | _ -> Error (a.loc, "Not int"))
  | Pred a -> (
      let* va = interpret_expr context a in
      match va with VInt i -> Ok (VInt (i - 1)) | _ -> Error (a.loc, "Not int"))
  | IsZero a -> (
      let* va = interpret_expr context a in
      match va with
      | VInt i -> Ok (VBool (i == 0))
      | _ -> Error (a.loc, "Not int"))
  | If (cond, yes, no) -> (
      let* vcond = interpret_expr context cond in
      match vcond with
      | VBool true -> interpret_expr context yes
      | VBool false -> interpret_expr context no
      | _ -> Error (cond.loc, "Not a bool"))

let interpretProgram context = function
  | PExpr e ->
      let* result = interpret_expr context e in
      Ok (string_of_value result, context)
  | PDef (name, body) ->
      let* body = interpret_expr context body in
      Ok (name, (name, body) :: context)

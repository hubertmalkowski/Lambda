open Syntax
open Lambda_utils.Utils

type typ =
  | TVar of string
  | TInt
  | TBool
  | TArrow of typ * typ
  | TTypeArrow of string * typ

and ctx = (string * typ) list

let rec string_of_typ = function
  | TVar a -> a
  | TInt -> "Int"
  | TBool -> "Bool"
  | TArrow (TArrow (arg1, ret1), ret) ->
      "(" ^ string_of_typ (TArrow (arg1, ret1)) ^ ")" ^ "->" ^ string_of_typ ret
  | TArrow (arg, ret) -> string_of_typ arg ^ "->" ^ string_of_typ ret
  | TTypeArrow (arg, ret) -> "∀" ^ arg ^ " " ^ string_of_typ ret

let rec typ_of_typ_sig = function
  | TSArrow (from, tto) -> TArrow (typ_of_typ_sig from, typ_of_typ_sig tto)
  | TS "Bool" -> TBool
  | TS "Int" -> TInt
  | TS x -> TVar x

let validate_type tvars tsig =
  let tt = typ_of_typ_sig tsig in
  match tt with
  | TVar x ->
      if List.mem x tvars then Ok (TVar x)
      else Error ("Not found type variable" ^ x)
  | others -> Ok others

let rec substitute (var, t) = function
  | TVar n -> if n = var then t else TVar n
  | TArrow (tfrom, tto) ->
      TArrow (substitute (var, t) tfrom, substitute (var, t) tto)
  | TBool -> TBool
  | TInt -> TInt
  (* Support shadowing  *)
  | TTypeArrow (param, ret) ->
      if param = var then TTypeArrow (param, ret)
      else TTypeArrow (param, substitute (var, t) ret)

let rec typecheck_expr tvars ctx e = match e.item with
  | Var n -> (
      match List.assoc_opt n ctx with
      | Some t -> Ok t
      | None -> Error ("Unbound variable " ^ n))
  | Lambda (paramName, typSig, body) ->
      let* tparam = validate_type tvars typSig in
      let* tbody = typecheck_expr tvars ((paramName, tparam) :: ctx) body in
      Ok (TArrow (tparam, tbody))
  | TypeLambda (n, ex) ->
      let* body = typecheck_expr (n :: tvars) ctx ex in
      Ok (TTypeArrow (n, body))
  | App (fn, arg) -> (
      let* tfn = typecheck_expr tvars ctx fn in
      let* targ = typecheck_expr tvars ctx arg in
      match tfn with
      | TArrow (tfrom, tto) ->
          if targ = tfrom then Ok tto else Error "Wrong argument tyupe"
      | _ -> Error "Not a function")
  | TApp (expr, typ_sig) -> (
      let* ttyp = validate_type tvars typ_sig in
      let* texpr = typecheck_expr tvars ctx expr in
      match texpr with
      | TTypeArrow (param, ret) -> Ok (substitute (param, ttyp) ret)
      | _ -> Error "Not a type arrow")
  | Let (name, value, bod) ->
      let* tval = typecheck_expr tvars ctx value in
      typecheck_expr tvars ((name, tval) :: ctx) bod
  | Int _ -> Ok TInt
  | Bool _ -> Ok TBool
  | Succ ex | Pred ex -> (
      let* texpr = typecheck_expr tvars ctx ex in
      match texpr with TInt -> Ok TInt | _ -> Error "Not an int")
  | IsZero ex -> (
      let* texpr = typecheck_expr tvars ctx ex in
      match texpr with TInt -> Ok TBool | _ -> Error "Not an int")
  | If (cond, yes, no) ->
      let* tcond = typecheck_expr tvars ctx cond in
      let* _ =
        if tcond = TBool then Ok () else Error "Condition must be of type bool"
      in
      let* tyes = typecheck_expr tvars ctx yes in
      let* tno = typecheck_expr tvars ctx no in
      if tyes = tno then Ok tyes else Error "BRANCHES MUST be the same type"

let typecheck_program ctx = function
  | PExpr e -> Result.map (fun x -> (x, ctx)) (typecheck_expr [] ctx e)
  | PDef (n, e) ->
      Result.map (fun x -> (x, (n, x) :: ctx)) (typecheck_expr [] ctx e)

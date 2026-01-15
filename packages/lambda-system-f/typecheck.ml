open Syntax
open Lambda_utils.Utils

type typ =
  | TVar of string
  | TInt
  | TBool
  | TArrow of typ * typ
  | TTypeArrow of string * typ

and ctx = { vars : (string * typ) list; tvars : string list }

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

let validate_type tvars tsig loc =
  let tt = typ_of_typ_sig tsig in
  match tt with
  | TVar x ->
      if List.mem x tvars then Ok (TVar x)
      else Error (loc, "Not found type variable " ^ x)
  | others -> Ok others

let rec substitute tvar tto = function
  | TVar a -> if tvar = a then tto else TVar a
  | TInt -> TInt
  | TBool -> TBool
  | TTypeArrow (tv, body) -> TTypeArrow (tv, substitute tvar tto body)
  | TArrow (tfrom, atto) ->
      TArrow (substitute tvar tto tfrom, substitute tvar tto atto)

let rec typecheck_expr ctx expr =
  match expr.item with
  | Var s -> (
      match List.assoc_opt s ctx.vars with
      | Some t -> Ok t
      | None -> Error (expr.loc, "Variable " ^ s ^ " wasn't found"))
  | Lambda (param, tsig, body) ->
      let* tparam = validate_type ctx.tvars tsig expr.loc in
      let* tbody =
        typecheck_expr { ctx with vars = (param, tparam) :: ctx.vars } body
      in
      Ok (TArrow (tparam, tbody))
  | App (lambda, param) -> (
      let* tlambda = typecheck_expr ctx lambda in
      let* tparam = typecheck_expr ctx param in
      match tlambda with
      | TArrow (from, tto) ->
          if tparam = from then Ok tto
          else
            Error
              ( param.loc,
                "Param of type " ^ string_of_typ tparam
                ^ " is not compatible with type " ^ string_of_typ from
                ^ " in function of type " ^ string_of_typ tlambda )
      | e ->
          Error
            ( lambda.loc,
              "Term of type " ^ string_of_typ e ^ " is not a function" ))
  | Let (name, value, body) ->
      let* tvalue = typecheck_expr ctx value in
      typecheck_expr { ctx with vars = (name, tvalue) :: ctx.vars } body
  | Int _ -> Ok TInt
  | Bool _ -> Ok TBool
  | Succ a | Pred a -> (
      let* t_a = typecheck_expr ctx a in
      match t_a with
      | TInt -> Ok TInt
      | e ->
          Error
            ( a.loc,
              "Expected: " ^ string_of_typ TInt ^ " Got: " ^ string_of_typ e ))
  | IsZero a -> (
      let* t_a = typecheck_expr ctx a in
      match t_a with
      | TInt -> Ok TBool
      | e ->
          Error
            ( a.loc,
              "Expected: " ^ string_of_typ TInt ^ " Got: " ^ string_of_typ e ))
  | If (cond, yes, no) -> (
      let* tcond = typecheck_expr ctx cond in
      let* tyes = typecheck_expr ctx yes in
      let* tno = typecheck_expr ctx no in
      match tcond with
      | TBool ->
          if tyes = tno then Ok tyes
          else
            Error
              ( expr.loc,
                "Branches do not match Expected. Yes type branch: "
                ^ string_of_typ tyes ^ ", No type branch: " ^ string_of_typ tno
              )
      | e ->
          Error
            ( cond.loc,
              "Expected: " ^ string_of_typ TBool ^ " Got: " ^ string_of_typ e ))
  | TypeLambda (typename, body) ->
      if List.mem typename ctx.tvars then
        Error (expr.loc, "Typevar " ^ typename ^ " already exists.")
      else
        let* tbody =
          typecheck_expr { ctx with tvars = typename :: ctx.tvars } body
        in
        Ok (TTypeArrow (typename, tbody))
  | TApp (type_lambda, type_param) -> (
      let* tlambda = typecheck_expr ctx type_lambda in
      let* tparam = validate_type ctx.tvars type_param expr.loc in
      match tlambda with
      | TTypeArrow (tvar, tbody) -> Ok (substitute tvar tparam tbody)
      | e ->
          Error (type_lambda.loc, string_of_typ e ^ " is not a type abstraction")
      )

let typecheckRepl ctx pl =
  match pl with
  | PExpr e ->
      let* res = typecheck_expr { vars = ctx; tvars = [] } e in
      Ok (res, ctx)
  | PDef (name, value) ->
      let* tvalue = typecheck_expr { vars = ctx; tvars = [] } value in
      Ok (tvalue, (name, tvalue) :: ctx)

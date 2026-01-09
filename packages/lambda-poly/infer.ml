open Syntax
open Lambda_utils.Utils

type typ = TVar of int | TInt | TBool | TArrow of typ * typ

type ctx = {
  vars : (string * (int list * typ)) list;
  typs : (string * typ) list;
  exceptions : (string * typ) list;
}

let rec typ_of_typ_sig typs = function
  | TSArrow (from, rest) ->
      let* tfrom = typ_of_typ_sig typs from in
      let* trest = typ_of_typ_sig typs rest in
      Ok (TArrow (tfrom, trest))
  | TS "Int" -> Ok TInt
  | TS "Bool" -> Ok TBool
  | TS var -> (
      match List.assoc_opt var typs with
      | Some t -> Ok t
      | None -> Error ("Invalid type " ^ var))

let int_to_alpha n =
  if n < 26 then String.make 1 (Char.chr (97 + n)) (* a-z *)
  else "t" ^ string_of_int n (* t0, t1, t2, ... *)

let rec string_of_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVar x -> "'" ^ int_to_alpha x
  | TArrow (TArrow (arg1, ret1), ret) ->
      "(" ^ string_of_typ (TArrow (arg1, ret1)) ^ ")" ^ "->" ^ string_of_typ ret
  | TArrow (arg, ret) -> string_of_typ arg ^ "->" ^ string_of_typ ret

let counter = ref 0

let fresh () =
  incr counter;
  TVar !counter

let rec sub subst = function
  | TVar x -> (
      match List.assoc_opt x subst with
      | Some t' -> sub subst t' (* Recursive: follow chains *)
      | None -> TVar x)
  | TInt -> TInt
  | TBool -> TBool
  | TArrow (t1, t2) -> TArrow (sub subst t1, sub subst t2)

let sub_many subst constrs =
  List.map (fun (t1, t2) -> (sub subst t1, sub subst t2)) constrs

let rec occurs v = function
  | TVar y -> v = y
  | TInt | TBool -> false
  | TArrow (t1, t2) -> occurs v t1 || occurs v t2

let rec unify = function
  | [] -> Ok []
  | (t1, t2) :: rest -> (
      match (t1, t2) with
      | TInt, TInt -> unify rest
      | TBool, TBool -> unify rest
      | TVar p, TVar q when p = q -> unify rest
      | TVar p, t | t, TVar p ->
          if occurs p t then
            Error
              ("Infinite type: " ^ string_of_typ (TVar p) ^ " in "
             ^ string_of_typ t)
          else
            let subst = [ (p, t) ] in
            let rest' = sub_many subst rest in
            let* rest_subst = unify rest' in
            let subst' = List.map (fun (x, t) -> (x, sub rest_subst t)) subst in
            Ok (subst' @ rest_subst)
      | TArrow (a1, r1), TArrow (a2, r2) -> unify ((a1, a2) :: (r1, r2) :: rest)
      | _ -> Error "Type mismatch")

let free_vars_in_type typ =
  let rec go = function
    | TVar x -> [ x ]
    | TInt | TBool -> []
    | TArrow (t1, t2) -> go t1 @ go t2
  in
  List.sort_uniq Int.compare (go typ)

let free_vars_in_ctx ctx =
  List.concat_map (fun (_, (_, typ)) -> free_vars_in_type typ) ctx

let rec expr_constraints ctx = function
  | Var n -> (
      match List.assoc_opt n ctx.vars with
      | Some (vars, t) ->
          let subst = List.map (fun v -> (v, fresh ())) vars in
          let instantiated = sub subst t in
          Ok (instantiated, [])
      | None -> Error ("unknown variable" ^ n))
  | Lambda (param, typsig, body) ->
      let* tparam =
        match Option.map (typ_of_typ_sig ctx.typs) typsig with
        | Some t -> t
        | None -> Ok (fresh ())
      in
      let* tbody, bodyconstr =
        expr_constraints
          { ctx with vars = (param, ([], tparam)) :: ctx.vars }
          body
      in
      Ok (TArrow (tparam, tbody), bodyconstr)
  | App (lambda, arg) ->
      let* tlambda, lambda_constr = expr_constraints ctx lambda in
      let* targ, arg_contsr = expr_constraints ctx arg in
      let treturn = fresh () in
      let lambda_constr = (tlambda, TArrow (targ, treturn)) :: lambda_constr in
      Ok (treturn, lambda_constr @ arg_contsr @ arg_contsr)
  | Int _ -> Ok (TInt, [])
  | Bool _ -> Ok (TBool, [])
  | Succ e | Pred e ->
      let* te, econstr = expr_constraints ctx e in
      Ok (TInt, (te, TInt) :: econstr)
  | IsZero e ->
      let* te, econstr = expr_constraints ctx e in
      Ok (TBool, (te, TInt) :: econstr)
  | If (cond, yes, no) ->
      let* tcond, condconstr = expr_constraints ctx cond in
      let* tyes, yesconstr = expr_constraints ctx yes in
      let* tno, noconstr = expr_constraints ctx no in
      let constraints =
        [ (tcond, TBool); (tyes, tno) ] @ condconstr @ yesconstr @ noconstr
      in
      Ok (tno, constraints)
  | Raise _ -> Ok (fresh (), [])
  | TryCatch (_, patterns) -> (
      let* tpatterns =
        List.map (fun (_, e) -> expr_constraints ctx e) patterns |> result_all
      in
      match tpatterns with
      | [] -> Error "Try catch must not be empty"
      | (tfirst, constrfirst) :: tail ->
          let constrs =
            List.map (fun (t, elconstr) -> (t, tfirst) :: elconstr) tail
            |> List.concat |> List.append constrfirst
          in
          Ok (tfirst, constrs))
  | Let (name, value, body) ->
      let* value_typ, value_constrs = expr_constraints ctx value in

      let* value_subst = unify value_constrs in
      let value_typ_solved = sub value_subst value_typ in

      let ctx_vars = free_vars_in_ctx ctx.vars in
      let value_vars = free_vars_in_type value_typ_solved in
      let generalizable =
        List.filter (fun v -> not (List.mem v ctx_vars)) value_vars
      in
      let vars' = (name, (generalizable, value_typ_solved)) :: ctx.vars in
      expr_constraints { ctx with vars = vars' } body

let infer_decl ctx = function
  | PDef (name, value) ->
      let* value_typ, value_constrs = expr_constraints ctx value in

      let* value_subst = unify value_constrs in
      let value_typ_solved = sub value_subst value_typ in

      let ctx_vars = free_vars_in_ctx ctx.vars in
      let value_vars = free_vars_in_type value_typ_solved in
      let generalizable =
        List.filter (fun v -> not (List.mem v ctx_vars)) value_vars
      in
      let vars' = (name, (generalizable, value_typ_solved)) :: ctx.vars in
      Ok ({ ctx with vars = vars' }, Some name, value_typ_solved)
  | PExpr expr ->
      let* t, constr = expr_constraints ctx expr in
      let* subs = unify constr in
      let res = sub subs t in
      Ok (ctx, None, res)
  | PTypeDef (n, tsig) ->
      let* ttyp = typ_of_typ_sig ctx.typs tsig in
      let newtyps = (n, ttyp) :: ctx.typs in
      Ok ({ ctx with typs = newtyps }, Some n, ttyp)
  | _ -> Error "Not handled yet"

let inferred_to_repl (c : ctx ref) = function
  | Ok (c', name, res) ->
      let varname = match name with Some n -> n | None -> "_" in
      c := c';
      varname ^ " : " ^ string_of_typ res
  | Error n -> n

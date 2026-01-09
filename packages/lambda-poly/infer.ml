open Syntax
open Lambda_utils.Utils

type typ =
  | TVar of int
  | TInt
  | TBool
  | TArrow of typ * typ
  | TUnit
  | TException of typ

type ctx = {
  vars : (string * (int list * typ * string list)) list;
  typs : (string * typ) list;
}

let unique lst =
  let rec aux seen = function
    | [] -> List.rev seen
    | x :: xs -> if List.mem x seen then aux seen xs else aux (x :: seen) xs
  in
  aux [] lst

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
  | TUnit -> "unit"
  | TException t -> "!" ^ string_of_typ t

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
  | TUnit -> TUnit
  | TException t -> TException (sub subst t)
  | TArrow (t1, t2) -> TArrow (sub subst t1, sub subst t2)

let sub_many subst constrs =
  List.map (fun (t1, t2) -> (sub subst t1, sub subst t2)) constrs

let rec occurs v = function
  | TVar y -> v = y
  | TInt | TBool | TUnit -> false
  | TException t -> occurs v t
  | TArrow (t1, t2) -> occurs v t1 || occurs v t2

let rec unify = function
  | [] -> Ok []
  | (t1, t2) :: rest -> (
      match (t1, t2) with
      | TInt, TInt -> unify rest
      | TBool, TBool -> unify rest
      | TUnit, TUnit -> unify rest
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
      | TException a, TException b -> unify ((a, b) :: rest)
      | a, b ->
          Error
            ("Type mismatch: " ^ string_of_typ a ^ " not compatible with "
           ^ string_of_typ b))

let free_vars_in_type typ =
  let rec go = function
    | TVar x -> [ x ]
    | TUnit | TInt | TBool -> []
    | TException t -> go t
    | TArrow (t1, t2) -> go t1 @ go t2
  in
  List.sort_uniq Int.compare (go typ)

let free_vars_in_ctx ctx =
  List.concat_map (fun (_, (_, typ, _)) -> free_vars_in_type typ) ctx

let rec expr_constraints ctx = function
  | Var n -> (
      match List.assoc_opt n ctx.vars with
      | Some (vars, t, eff) ->
          let subst = List.map (fun v -> (v, fresh ())) vars in
          let instantiated = sub subst t in
          Ok (instantiated, [], eff)
      | None -> Error ("unknown variable" ^ n))
  | Lambda (param, typsig, body) ->
      let* tparam =
        match Option.map (typ_of_typ_sig ctx.typs) typsig with
        | Some t -> t
        | None -> Ok (fresh ())
      in
      let* tbody, bodyconstr, effects =
        expr_constraints
          { ctx with vars = (param, ([], tparam, [])) :: ctx.vars }
          body
      in
      Ok (TArrow (tparam, tbody), bodyconstr, effects)
  | App (lambda, arg) ->
      let* tlambda, lambda_constr, effects = expr_constraints ctx lambda in
      let* targ, arg_contsr, arg_effects = expr_constraints ctx arg in
      let treturn = fresh () in
      let lambda_constr = (tlambda, TArrow (targ, treturn)) :: lambda_constr in
      Ok
        ( treturn,
          lambda_constr @ arg_contsr @ arg_contsr,
          unique effects @ arg_effects )
  | Int _ -> Ok (TInt, [], [])
  | Bool _ -> Ok (TBool, [], [])
  | Succ e | Pred e ->
      let* te, econstr, effects = expr_constraints ctx e in
      Ok (TInt, (te, TInt) :: econstr, effects)
  | IsZero e ->
      let* te, econstr, eff = expr_constraints ctx e in
      Ok (TBool, (te, TInt) :: econstr, eff)
  | If (cond, yes, no) ->
      let* tcond, condconstr, effcond = expr_constraints ctx cond in
      let* tyes, yesconstr, yescond = expr_constraints ctx yes in
      let* tno, noconstr, nocond = expr_constraints ctx no in
      let constraints =
        [ (tcond, TBool); (tyes, tno) ] @ condconstr @ yesconstr @ noconstr
      in
      Ok (tno, constraints, unique effcond @ yescond @ nocond)
  | Raise (name, expr) -> (
      let exception_type = List.assoc_opt name ctx.typs in
      match exception_type with
      | None -> Error ("Unknown exception: " ^ name)
      | Some exc_type -> (
          match expr with
          | None -> Ok (fresh (), [ (exc_type, TException TUnit) ], [ name ])
          | Some e -> (
              match expr_constraints ctx e with
              | Error err -> Error err
              | Ok (texpr, constr, []) ->
                  Ok (fresh (), (texpr, exc_type) :: constr, [ name ])
              | Ok (_, _, _effects) ->
                  Error "Exception types must have no effects")))
  | TryCatch (_, patterns) -> (
      let* tpatterns =
        List.map (fun (_, e) -> expr_constraints ctx e) patterns |> result_all
      in
      match tpatterns with
      | [] -> Error "Try catch must not be empty"
      | (tfirst, constrfirst, eff) :: tail ->
          let constrs =
            List.map (fun (t, elconstr, _) -> (t, tfirst) :: elconstr) tail
            |> List.concat |> List.append constrfirst
          in
          Ok (tfirst, constrs, eff))
  | Let (name, value, body) ->
      let* value_typ, value_constrs, eff = expr_constraints ctx value in

      let* value_subst = unify value_constrs in
      let value_typ_solved = sub value_subst value_typ in

      let ctx_vars = free_vars_in_ctx ctx.vars in
      let value_vars = free_vars_in_type value_typ_solved in
      let generalizable =
        List.filter (fun v -> not (List.mem v ctx_vars)) value_vars
      in
      let vars' = (name, (generalizable, value_typ_solved, eff)) :: ctx.vars in
      expr_constraints { ctx with vars = vars' } body

let infer_decl ctx = function
  | PDef (name, value) ->
      let* value_typ, value_constrs, eff = expr_constraints ctx value in

      let* value_subst = unify value_constrs in
      let value_typ_solved = sub value_subst value_typ in

      let ctx_vars = free_vars_in_ctx ctx.vars in
      let value_vars = free_vars_in_type value_typ_solved in
      let generalizable =
        List.filter (fun v -> not (List.mem v ctx_vars)) value_vars
      in
      let vars' = (name, (generalizable, value_typ_solved, eff)) :: ctx.vars in
      Ok ({ ctx with vars = vars' }, Some name, value_typ_solved, eff)
  | PExpr expr ->
      let* t, constr, eff = expr_constraints ctx expr in
      let* subs = unify constr in
      let res = sub subs t in
      Ok (ctx, None, res, eff)
  | PTypeDef (n, tsig) ->
      let* ttyp = typ_of_typ_sig ctx.typs tsig in
      let newtyps = (n, ttyp) :: ctx.typs in
      Ok ({ ctx with typs = newtyps }, Some n, ttyp, [])
  | PExceptionDef (name, tsig) ->
      let* etyp =
        match Option.map (typ_of_typ_sig ctx.typs) tsig with
        | Some t -> t
        | None -> Ok TUnit
      in
      let newexc = (name, TException etyp) :: ctx.typs in
      Ok ({ ctx with typs = newexc }, Some name, TException etyp, [])

let print_exceptions = function
  | [] -> ""
  | head :: [] -> " raises " ^ head
  | exceptions -> " raises " ^ String.concat " | " exceptions

let inferred_to_repl (c : ctx ref) = function
  | Ok (c', name, res, eff) ->
      let varname = match name with Some n -> n | None -> "_" in
      c := c';
      varname ^ " : " ^ string_of_typ res ^ print_exceptions eff
  | Error n -> n

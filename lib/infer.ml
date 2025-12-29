open Ast
open Utils

type typ = 
    | TInt
    | TBool
    | TTuple of typ list
    | TVar of int
    | TArrow of typ * typ
and scheme = Forall of int list * typ
and type_ctx = (string * scheme) list
and constr = (typ * typ) list


let rec typ_of_typ_sig = function
    | TSArrow (from, rest) -> 
        let* tfrom = typ_of_typ_sig from in
        let* trest = typ_of_typ_sig rest in
        Ok (TArrow (tfrom, trest) )
    | TSTuple typs -> (List.map typ_of_typ_sig typs) |> result_all_map (fun a -> TTuple a)
    | TS "Int" -> Ok (TInt)
    | TS "Bool" -> Ok (TBool)
    | TS _ -> Error ("unknown")

let int_to_alpha n =
    if n < 26 then
        String.make 1 (Char.chr (97 + n))  (* a-z *)
    else
        "t" ^ string_of_int n  (* t0, t1, t2, ... *)

let rec string_of_typ = function
    | TInt -> "int"
    | TBool -> "bool"
    | TTuple typs -> "{" ^ (String.concat ", " (List.map string_of_typ typs)) ^ "}"
    | TVar x -> "'" ^ int_to_alpha x
    | TArrow (TArrow (arg1, ret1), ret) -> "(" ^ string_of_typ (TArrow (arg1, ret1)) ^ ")" ^ "->" ^ string_of_typ ret
    | TArrow (arg, ret) ->  string_of_typ arg ^ "->" ^ string_of_typ ret


let counter = ref 0
let fresh () = incr counter; TVar !counter



let rec occurs v = function
    | TVar y -> v = y
    | TInt | TBool -> false
    | TArrow (t1, t2) -> occurs v t1 || occurs v t2
    | TTuple ts -> List.exists (occurs v) ts



let rec substitute_single subst = function
        | TVar x -> (match List.assoc_opt x subst with
            | Some t' -> substitute_single subst t'  (* Recursive: follow chains *)
            | None -> TVar x)
        | TInt -> TInt
        | TBool -> TBool
        | TArrow (t1, t2) -> TArrow (substitute_single subst t1, substitute_single subst t2)
        | TTuple ts -> TTuple (List.map (substitute_single subst) ts)

let substitute subst constrs =
    List.map (fun (t1, t2) -> (substitute_single subst t1, substitute_single subst t2)) constrs

    

let rec unify = function
    | [] -> Ok []  
    | (t1, t2) :: rest -> (match (t1, t2) with
        | (TInt, TInt) -> unify rest
        | (TBool, TBool) -> unify rest
        | (TVar p, TVar q) when p = q -> unify rest
        | (TVar p, t) | (t, TVar p) ->
            if occurs p t then
                Error ("Infinite type: " ^ string_of_typ (TVar p) ^ " in " ^ string_of_typ t) 
            else
            let subst = [(p, t)] in
            let rest' = substitute subst rest in
            let* rest_subst = unify rest' in
            let subst' = List.map (fun (x, t) -> (x, substitute_single rest_subst t)) subst in
            Ok (subst' @ rest_subst)
        | (TArrow (a1, r1), TArrow (a2, r2)) ->
            unify ((a1, a2) :: (r1, r2) :: rest)
        | (TTuple ts1, TTuple ts2) ->
            if List.length ts1 <> List.length ts2 then
                Error "Tuple size mismatch"
            else
                let new_constrs = List.combine ts1 ts2 in
                unify (new_constrs @ rest)
         | _ -> Error "Type mismatch")

let free_vars_in_type typ =
    let rec go = function
        | TVar x -> [x]
        | TInt | TBool -> []
        | TArrow(t1, t2) -> go t1 @ go t2
        | TTuple ts -> List.concat_map go ts
    in
    List.sort_uniq Int.compare (go typ)

let free_vars_in_ctx ctx =
    List.concat_map (fun (_, (_, typ)) -> free_vars_in_type typ) ctx

let rec constraints ctx = function
    | Var n -> (match List.assoc_opt n ctx with 
        | Some (vars, t) -> 
            let subst = List.map (fun v -> (v, fresh())) vars in
            let instantiated = substitute_single subst t in
            Ok (instantiated, [])
        | None -> Error ("unknown variable" ^ n)
    )
    | Let (name, value, body) -> 
        let* (value_typ, value_constrs) = constraints ctx value in
        
        let* value_subst = unify value_constrs in
        let value_typ_solved = substitute_single value_subst value_typ in
        
        let ctx_vars = free_vars_in_ctx ctx in
        let value_vars = free_vars_in_type value_typ_solved in
        let generalizable = List.filter (fun v -> 
            not (List.mem v ctx_vars)
        ) value_vars in
        
        let ctx' = (name, (generalizable, value_typ_solved)) :: ctx in
        
        constraints ctx' body
    | Lambda (param_name, type_sig, body) ->
        let param = fresh () in
        let* param_sig_constr = (match type_sig with
            | Some x -> let* typ = typ_of_typ_sig x in Ok [(param, typ)] 
            | None -> Ok []
        ) in
        (* Add monomorphic binding (no generalization in lambda) *)
        let ctx' = (param_name, ([], param)) :: ctx in
        let* (body_infer, body_constr) = constraints ctx' body in
        Ok (TArrow (param, body_infer), body_constr @ param_sig_constr)
    | App (f, arg) -> 
        let* (f_typ, f_constrs) = constraints ctx f in
        let* (arg_typ, arg_constr) = constraints ctx arg in
        let result_typ = fresh () in
        let new_constr = (f_typ, TArrow (arg_typ, result_typ)) in
        let all_constrs = f_constrs @ arg_constr @ [new_constr] in
        Ok (result_typ, all_constrs)
    | Pred (arg)
    | Succ (arg) ->
        let* (arg_typ, arg_constr) = constraints ctx arg in
        let new_arg_constr = (arg_typ, TInt) in
        Ok (TInt, new_arg_constr :: arg_constr)
    | IsZero (arg) ->
        let* (arg_typ, arg_constr) = constraints ctx arg in
        let new_arg_constr = (arg_typ, TInt) in
        Ok (TBool, new_arg_constr :: arg_constr)
    | If (cond, yes, no) ->
        let* (cond_typ, cond_constr) = constraints ctx cond in
        let* (yes_typ, yes_constr) = constraints ctx yes in
        let* (no_typ, no_constr) = constraints ctx no in
        let cond_bool_constr = (cond_typ, TBool) in
        let branch_constr = (no_typ, yes_typ) in
        Ok (no_typ, cond_constr @ yes_constr @ no_constr @ [cond_bool_constr; branch_constr])
    | Int _ -> Ok (TInt, [])
    | Bool _ -> Ok (TBool, [])
    | Tuple exprs ->
        let* results = List.map (constraints ctx) exprs |> result_all in
        let types = List.map fst results in
        let all_constrs = List.concat_map snd results in
        Ok (TTuple types, all_constrs)
    | TupleProj (tuple_expr, index) ->
        let* (tuple_typ, tuple_constrs) = constraints ctx tuple_expr in
        let elem_vars = List.init (index + 1) (fun _ -> fresh()) in
        let result_typ = List.nth elem_vars index in
        let tuple_constr = (tuple_typ, TTuple elem_vars) in
        Ok (result_typ, tuple_constr :: tuple_constrs)


let typecheck syntax =
    let* (t, expr_constraints) = constraints [] syntax in
    let* sub = unify expr_constraints in
    Ok (substitute_single sub t)


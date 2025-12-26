open Ast
open Utils

type typ = 
    | TInt
    | TBool
    | TTuple of typ list
    | TVar of int
    | TArrow of typ * typ
and type_ctx = (string * typ) list
and constr = (typ * typ) list

let counter = ref 0
let fresh_var () =
  let n = !counter in
  counter := n + 1;
  TVar n


let rec typ_of_typ_sig = function
    | TSArrow (from, rest) -> 
        let* tfrom = typ_of_typ_sig from in
        let* trest = typ_of_typ_sig rest in
        Ok (TArrow (tfrom, trest) )
    | TSTuple typs -> (List.map typ_of_typ_sig typs) |> result_all_map (fun a -> TTuple a)
    | TS "Int" -> Ok (TInt)
    | TS "Bool" -> Ok (TBool)
    | TS _ -> Error ("unknown")

(* let rec typecheck ctx = function *)
(*     | Var var -> (match List.assoc_opt var ctx with  *)
(*             | Some x -> Ok x *)
(*             | None -> Error "Unbound variable" *)
(*         ) *)
(*     | Lambda (param, param_sig, expr) ->   *)
(*         let* param_typ = typ_of_typ_sig param_sig in *)
(*         let* body_typ = typecheck ((param, param_typ) :: ctx) expr in *)
(*         Ok (TArrow (param_typ, body_typ)) *)
(*     | App (lamb, arg) ->  *)
(*         let* (args, ret) = assert_is_arrow ctx lamb in *)
(*         let* param_typ = typecheck ctx arg in *)
(*         if param_typ = args then Ok ret else Error "Wrong argument type" *)
(*     | Int _ -> Ok TInt *)
(*     | Bool _ -> Ok TBool *)
(*     | Succ expr  *)
(*     | Pred expr -> assert_is_int ctx expr >>= (fun _ -> Ok TInt) *)
(*     | IsZero expr -> assert_is_int ctx expr >>= (fun _ -> Ok TBool) *)
(*     | Tuple exprs -> List.map (typecheck ctx) exprs |> result_all_map (fun a -> TTuple a) *)
(*     | TupleProj (tup, idx) ->  *)
(*         let* ttup = assert_is_tuple ctx tup in *)
(*         (match List.nth_opt ttup idx with *)
(*             | Some el -> Ok el *)
(*             | None -> Error "No element in tuple" *)
(*         ) *)
(*     | If (cond, yes, no) ->  *)
(*         let* _ = assert_is_bool ctx cond in *)
(*         let* tyes = typecheck ctx yes in *)
(*         let* tno = typecheck ctx no in *)
(*         if tyes = tno then Ok tyes else Error "Then branch doesn't match Else branch" *)
(*     | Let (name, value, body) ->  *)
(*         let* tvalue = typecheck ctx value in *)
(*         typecheck ((name, tvalue) :: ctx) body *)
(* and assert_is_arrow ctx expr = *)
(*     let* typ = typecheck ctx expr in *)
(*     match typ with *)
(*     | TArrow (arg, ret) -> Ok( (arg, ret) ) *)
(*     | _ -> Error "Not a function" *)
(* and assert_is_int ctx expr =  *)
(*     let* typ = typecheck ctx expr in *)
(*     match typ with *)
(*     | TInt -> Ok TInt *)
(*     | _ -> Error "Not an int" *)
(* and assert_is_bool ctx expr =  *)
(*     let* typ = typecheck ctx expr in *)
(*     match typ with *)
(*     | TBool -> Ok TBool *)
(*     | _ -> Error "Not a bool" *)
(* and assert_is_tuple ctx expr =  *)
(*     let* typ = typecheck ctx expr in *)
(*     match typ with *)
(*     | TTuple l -> Ok l *)
(*     | _ -> Error "Not a tuple" *)


let rec typecheck2 ctx = function
    | Var var -> (match List.assoc_opt var ctx with 
            | Some x -> Ok (x, [])
            | None -> Error "Unbound variable"
        )
    | Lambda (param, _, body) ->
        let param_typ = fresh_var () in
        let* (body_typ, body_constr) = typecheck2 ((param, param_typ) :: ctx) body in
        Ok (TArrow (param_typ, body_typ), body_constr)
    | App (f, arg) -> 
        let* (f_typ, f_constrs) = typecheck2 ctx f in
        let* (arg_typ, arg_constr) = typecheck2 ctx arg in
        let result_typ = fresh_var () in
        let new_constr = (f_typ, TArrow (arg_typ, result_typ)) in
        let all_constrs = f_constrs @ arg_constr @ [new_constr] in
        Ok (result_typ, all_constrs)
    | Pred (arg)
    | Succ (arg) ->
        let* (arg_typ, arg_constr) = typecheck2 ctx arg in
        let new_arg_constr = (arg_typ, TInt) in
        Ok (TInt, new_arg_constr :: arg_constr)
    | IsZero (arg) ->
        let* (arg_typ, arg_constr) = typecheck2 ctx arg in
        let new_arg_constr = (arg_typ, TInt) in
        Ok (TBool, new_arg_constr :: arg_constr)
    | Let (name, value, body) ->
        let* (val_typ, val_constr) = typecheck2 ctx value in
        let* (body_typ, body_constr) = typecheck2 ((name, val_typ) :: ctx) body in
        Ok (body_typ, val_constr @ body_constr)
    | If (cond, yes, no) ->
        let* (cond_typ, cond_constr) = typecheck2 ctx cond in
        let* (yes_typ, yes_constr) = typecheck2 ctx yes in
        let* (no_typ, no_constr) = typecheck2 ctx no in
        let cond_bool_constr = (cond_typ, TBool) in
        let branch_constr = (no_typ, yes_typ) in
        Ok (no_typ, cond_constr @ yes_constr @ no_constr @ [cond_bool_constr; branch_constr])
    | Int _ -> Ok (TInt, [])
    | Bool _ -> Ok (TBool, [])
    | _ -> Error "" 

type substitution = (int * typ) list  (* Maps type variables to types *)


let rec occurs (x : int) (t : typ) : bool =
  match t with
  | TVar y -> x = y
  | TInt | TBool -> false
  | TArrow (t1, t2) -> occurs x t1 || occurs x t2
  | TTuple ts -> List.exists (occurs x) ts

let rec apply_subst (subst : substitution) (t : typ) : typ =
  match t with
  | TVar x -> 
      (match List.assoc_opt x subst with
       | Some t' -> apply_subst subst t'  (* Recursive: follow chains *)
       | None -> TVar x)
  | TInt -> TInt
  | TBool -> TBool
  | TArrow (t1, t2) -> TArrow (apply_subst subst t1, apply_subst subst t2)
  | TTuple ts -> TTuple (List.map (apply_subst subst) ts)

let apply_subst_to_constrs (subst : substitution) (constrs : constr) : constr =
  List.map (fun (t1, t2) -> (apply_subst subst t1, apply_subst subst t2)) constrs

let rec unify = function
        | [] -> Ok []
        | (t1, t2) :: rest -> 
            (match (t1, t2) with
                | (TInt, TInt) -> unify rest
                | (TBool, TBool) -> unify rest
                | (TVar x, t) | (t, TVar x) ->
                    if occurs x t then
                      Error "Infinite type"
                    else
                      (* Create substitution [x -> t] *)
                      let subst = [(x, t)] in
                      (* Apply this substitution to remaining constraints *)
                      let rest' = apply_subst_to_constrs subst rest in
                      (* Solve rest, then combine substitutions *)
                      let* rest_subst = unify rest' in
                      Ok (subst @ rest_subst)
               | (TArrow (a1, r1), TArrow (a2, r2)) ->
                      unify ((a1, a2) :: (r1, r2) :: rest)
               | (TTuple ts1, TTuple ts2) ->
                      if List.length ts1 <> List.length ts2 then
                        Error "Tuple size mismatch"
                      else
                      let new_constrs = List.combine ts1 ts2 in
                      unify (new_constrs @ rest)
      
                | _ -> Error "Type mismatch")

let infer expr =
  let* (typ, constrs) = typecheck2 [] expr in
  let* subst = unify constrs in
  Ok (apply_subst subst typ)

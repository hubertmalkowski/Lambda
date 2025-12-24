open Ast

type typ = 
    | TInt
    | TBool
    | TArrow of typ * typ
and type_ctx = (string * typ) list

let (>>=) r f = Result.bind r f

let (let*) = Result.bind

let rec typ_of_typ_sig = function
    | TSArrow (from, rest) -> 
        let* tfrom = typ_of_typ_sig from in
        let* trest = typ_of_typ_sig rest in
        Ok (TArrow (tfrom, trest) )
    | TS "Int" -> Ok (TInt)
    | TS "Bool" -> Ok (TBool)
    | TS _ -> Error ("unknown")

let rec typecheck ctx = function
    | Var var -> (match List.assoc_opt var ctx with 
            | Some x -> Ok x
            | None -> Error "Unbound variable"
        )
    | Lambda (param, param_sig, expr) ->  
        let* param_typ = typ_of_typ_sig param_sig in
        let* body_typ = typecheck ((param, param_typ) :: ctx) expr in
        Ok (TArrow (param_typ, body_typ))
    | App (lamb, arg) -> 
        let* (args, ret) = assert_is_arrow ctx lamb in
        let* param_typ = typecheck ctx arg in
        if param_typ = args then Ok ret else Error "Wrong argument type"
    | Int _ -> Ok TInt
    | Bool _ -> Ok TBool
    | Succ expr 
    | Pred expr -> assert_is_int ctx expr >>= (fun _ -> Ok TInt)
    | IsZero expr -> assert_is_int ctx expr >>= (fun _ -> Ok TBool)
    | If (cond, yes, no) -> 
        let* _ = assert_is_bool ctx cond in
        let* tyes = typecheck ctx yes in
        let* tno = typecheck ctx no in
        if tyes = tno then Ok tyes else Error "Then branch doesn't match Else branch"
    | Let (name, value, body) -> 
        let* tvalue = typecheck ctx value in
        typecheck ((name, tvalue) :: ctx) body

and assert_is_arrow ctx expr =
    let* typ = typecheck ctx expr in
    match typ with
    | TArrow (arg, ret) -> Ok( (arg, ret) )
    | _ -> Error "Not a function"
and assert_is_int ctx expr = 
    let* typ = typecheck ctx expr in
    match typ with
    | TInt -> Ok TInt
    | _ -> Error "Not an int"

and assert_is_bool ctx expr = 
    let* typ = typecheck ctx expr in
    match typ with
    | TBool -> Ok TBool
    | _ -> Error "Not a bool"

            

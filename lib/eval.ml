open Ast



(* EVALUATE BY VALUE *)
type value_cbv =
  | VInt of int
  | VBool of bool
  | VClosure of string * Ast.expr * env_cbv
and env_cbv = (string * value_cbv) list

let string_of_cbv = function
    | VInt a ->  string_of_int a
    | VBool a -> string_of_bool a
    | VClosure (param, body, _) -> Printf.sprintf "fun %s -> %s" param  (Ast.string_of_expr body)

let string_of_cbv_result = function 
    | Ok res -> string_of_cbv res
    | Error err -> err

let (>>=) r f = Result.bind r f

let rec strictEval env = function
        | Var x -> (match List.assoc_opt x env with
            | Some x -> Ok x
            | None -> Error "Not found")
        | Lambda (param, _, body) -> Ok (VClosure (param, body, env))
        | App (fn, arg) -> (match (strictEval env fn) with
                | Ok VClosure (param, body, env') -> 
                    (strictEval env arg) 
                    >>= fun x -> strictEval ((param, x) :: env') body
                    
                | Ok _ -> Error "Not a fun"
                | Error x -> Error x
            )
        | Let (name, value, body) -> strictEval env value 
                                    >>= (fun var -> strictEval ((name, var) :: env) body)
        | Int a -> Ok (VInt a)
        | Bool a -> Ok (VBool a)
        | Succ a -> (match strictEval env a  with
                        | Ok VInt a -> Ok (VInt (a + 1))
                        | Ok _ -> Error "not an int"
                        | Error x -> Error x
            )
        | Pred a -> (match strictEval env a  with
                        | Ok VInt a -> Ok (VInt (a - 1))
                        | Ok _ -> Error "not an int"
                        | Error x -> Error x
            )
        | IsZero a -> (match strictEval env a  with
                        | Ok VInt a -> Ok (VBool (a == 0))
                        | Ok _ -> Error "not an int"
                        | Error x -> Error x
            )
        | If (cond, yes, no) -> strictEval env cond 
                >>= (function
                | VBool a -> if a then strictEval env yes else strictEval env no
                | _ -> Error "Not a bool"
        )

type value_cbn = 
  | VInt of int
  | VBool of bool
  | VClosure of string * Ast.expr * env_cbn
  | VThunk of expr *  env_cbn * value_cbn option ref
  and env_cbn = (string * value_cbn) list

let string_of_cbn = function
    | VInt a ->  string_of_int a
    | VBool a -> string_of_bool a
    | VClosure (param, body, _) -> Printf.sprintf "fun %s -> %s" param  (Ast.string_of_expr body)
    | VThunk _ -> "THUNK ????"

let string_of_cbn_result = function 
    | Ok res -> string_of_cbn res
    | Error err -> err

let (let*) = Result.bind

let rec lazyEval env = 
    function
    | Var name -> (match List.assoc_opt name env with
        | Some VThunk (expr, thunk_env, cache) -> evalThunk (expr, thunk_env, cache)
        | Some el -> Ok el
        | None -> Error "not found" 
    )
    | Lambda (param, _, expr) -> Ok (VClosure (param, expr, env))
    | App (lamb, arg_expr) ->     
        let* closure = lazyEval env lamb in
        (match closure with 
            | VClosure (paramName, expr, closure_env) -> 
                let arg_thunk = VThunk (arg_expr, env, ref None) in
                lazyEval ((paramName, arg_thunk) :: closure_env) expr
            | _ -> Error "Not a fun" 
    )
    | Let (name, value, body) -> 
        let thunk = VThunk (value, env, ref None) in
        lazyEval ((name, thunk) :: env) body
    | Int a -> Ok (VInt a)
    | Bool b -> Ok (VBool b)
    | Succ a -> (match lazyEval env a  with
                    | Ok VInt a -> Ok (VInt (a + 1))
                    | Ok _ -> Error "not an int"
                    | Error x -> Error x
            )
    | Pred a -> (match lazyEval env a  with
                    | Ok VInt a -> Ok (VInt (a - 1))
                    | Ok _ -> Error "not an int"
                    | Error x -> Error x)
    | IsZero a -> (match lazyEval env a  with
                    | Ok VInt a -> Ok (VBool (a == 0))
                    | Ok _ -> Error "not an int"
                    | Error x -> Error x)

    | If (cond, yes, no) -> lazyEval env cond 
                >>= (function
                | VBool a -> if a then lazyEval env yes else lazyEval env no
                | _ -> Error "Not a bool")

and evalThunk (expr, thunk_env, cache) = match !cache with 
        | Some v-> Ok v
        | None -> (match lazyEval thunk_env expr with
            | Ok v -> 
                cache := Some v; 
                Ok v
            | el -> el)




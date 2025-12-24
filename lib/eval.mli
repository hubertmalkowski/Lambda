type value_cbv =
    VInt of int
  | VBool of bool
  | VClosure of string * Ast.expr * env_cbv
and env_cbv = (string * value_cbv) list
val string_of_cbv : value_cbv -> string
val string_of_cbv_result : (value_cbv, string) result -> string
val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
val strictEval : env_cbv -> Ast.expr -> (value_cbv, string) result
type value_cbn =
    VInt of int
  | VBool of bool
  | VClosure of string * Ast.expr * env_cbn
  | VThunk of Ast.expr * env_cbn * value_cbn option ref
and env_cbn = (string * value_cbn) list
val string_of_cbn : value_cbn -> string
val string_of_cbn_result : (value_cbn, string) result -> string
val ( let* ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
val lazyEval : env_cbn -> Ast.expr -> (value_cbn, string) result
val evalThunk :
  Ast.expr * env_cbn * value_cbn option ref -> (value_cbn, string) result

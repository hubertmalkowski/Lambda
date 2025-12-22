{
open Parser

exception SyntaxError of string
}

let white = [' ' '\t' '\n' '\r']+
let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let int = '-'? digit+
let id = letter (letter | digit | '_' | '\'')*

rule token = parse
  | white       { token lexbuf }
  | ';'         { comment lexbuf }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | "->"        { ARROW }
  | '='         { EQUAL }
  | "fun"       { FUN }
  | "let"       { LET }
  | "in"        { IN }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "iszero"    { ISZERO }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | int as n    { INT (int_of_string n) }
  | id as s     { ID s }
  | eof         { EOF }
  | _ as c      { raise (SyntaxError ("Unexpected character: " ^ String.make 1 c)) }

and comment = parse
  | '\n'        { token lexbuf }
  | eof         { EOF }
  | _           { comment lexbuf }

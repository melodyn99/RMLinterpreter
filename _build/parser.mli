
(* The type of tokens. *)

type token = 
  | WITH
  | WILDCARD
  | VAR of (Types.var)
  | TRUE
  | TO
  | THEN
  | SUB
  | STR of (string)
  | SPAWN
  | SEND
  | SEMICOLON
  | RPAREN
  | RETURN
  | REF
  | RECV
  | REC
  | RBRACK
  | PICK
  | OR
  | NOT
  | NE
  | MUL
  | MOD
  | MATCH
  | LT
  | LPAREN
  | LET
  | LE
  | LBRACK
  | JOIN
  | INT of (int)
  | IN
  | IF
  | GT
  | GE
  | FUN
  | FALSE
  | EQ
  | EOF
  | END
  | ELSE
  | DIV
  | DEREF
  | CONS
  | COMMA
  | CAT
  | CASE
  | BEGIN
  | AWAIT
  | ASSIGN
  | ARROW
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.exp)

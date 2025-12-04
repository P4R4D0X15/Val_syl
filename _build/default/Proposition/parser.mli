
(* The type of tokens. *)

type token = 
  | TOP
  | PARO
  | PARF
  | OU
  | NON
  | IMP
  | ET
  | EOF
  | BOT
  | ATOME of (string)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val f: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Formule.formule)

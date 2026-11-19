{
open Parser
}

let white = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | "True" { TRUE }
  | "False" { FALSE }
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "Let" { LET }
  | "=" { EQUALS }
  | "In" { IN }
  | "If" { IF }
  | "Then" { THEN }
  | "Else" { ELSE }
  
  (* Additions: *)
  | "And" {AND}
  | "Or" {OR}
  | "Not" {NOT}
  | "First" {FIRST}
  | "Second" {SECOND}
  | "Fun" {FUN}
  | "Fix" {FIX}

  | ":" {COLON}
  | "." {PERIOD}
  | "," {COMMA}
  | "->" {ARROW}

  | "Int" {TYPEINT}
  | "Bool" {TYPEBOOL}
  
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
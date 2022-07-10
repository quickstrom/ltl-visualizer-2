{
open Parser
open Error
}

let atomic = ['A'-'Z']
let ident = ['a'-'z' '&' '|' '-' '>' '<']+
let whitespace = [' ' '\t' '\n']

rule f = parse
  | whitespace+ { f lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | atomic as a { ATOMIC a }
  | "true" { TOP }
  | "false" { BOTTOM }
  | ident as a { IDENT a }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }
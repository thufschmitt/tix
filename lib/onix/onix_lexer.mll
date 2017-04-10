{
  open Onix_parser
}

let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let number = [ '0'-'9' ]+

rule read =
  parse
  | white {read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | "or" { OR_KW }
  | "rec" { REC_KW }
  | "inherit" { INHERIT_KW }
  | number { INTEGER (int_of_string @@ Lexing.lexeme lexbuf)}
  | '.' { DOT }
  | ',' { COMMA }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | '}' { BRACE_R }
  | '{' { BRACE_L }
  | '(' { PAREN_L }
  | ')' { PAREN_R }
  | '?' { QUESTION_MARK }
  | '=' { EQUAL }
  | '@' { AROBASE }
  | "${" { DOLLAR_BRACE }
  | id { ID (Lexing.lexeme lexbuf ) }
  | eof { EOF }
  | _ { failwith "unknown token" }

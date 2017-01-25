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
  | number { INTEGER (int_of_string @@ Lexing.lexeme lexbuf)}
  | '.' { DOT }
  | ',' { COMMA }
  | ':' { COLON }
  | '}' { BRACE_R }
  | '{' { BRACE_L }
  | '?' { QUESTION_MARK }
  | "${" { DOLLAR_BRACE }
  | id { ID (Lexing.lexeme lexbuf ) }
  | eof { EOF }
  | _ { failwith "unknown token" }

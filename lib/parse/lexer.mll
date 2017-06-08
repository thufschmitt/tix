{
  open Parser

let string_buffer = Buffer.create 128

let reset_string_buffer () = Buffer.clear string_buffer
let add_char = Buffer.add_char string_buffer
let get_string () = Buffer.contents string_buffer
}

let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let number = [ '0'-'9' ]+
let brace_open_close = '{' white? '}'
let boolean = "true" | "false"

rule read =
  parse
  | white {read lexbuf }
  | newline { Lexing.new_line lexbuf; read lexbuf }
  | "or" { OR_KW }
  | "rec" { REC_KW }
  | "inherit" { INHERIT_KW }
  | "let" { LET_KW }
  | "where" { WHERE_KW }
  | "if" { IF_KW}
  | "then" { THEN_KW }
  | "else" { ELSE_KW }
  | "in" { IN_KW }
  | "Cons" { CONS_KW }
  | number { INTEGER (int_of_string @@ Lexing.lexeme lexbuf)}
  | boolean { BOOL (bool_of_string @@ Lexing.lexeme lexbuf) }
  | '.' { DOT }
  | '-' { MINUS }
  | '+' { PLUS }
  | ',' { COMMA }
  | ':' { COLON }
  | ';' { SEMICOLON }
  | brace_open_close { BRACE_LR }
  | '}' { BRACE_R }
  | '{' { BRACE_L }
  | '(' { PAREN_L }
  | ')' { PAREN_R }
  | '[' { BRACKET_L }
  | ']' { BRACKET_R }
  | "/*:" { TY_START }
  | "*/" { TY_END }
  | "->" { ARROW_R }
  | '?' { QUESTION_MARK }
  | '=' { EQUAL }
  | "==" { DBL_EQUAL }
  | '@' { AROBASE }
  | '&' { AMPERSAND }
  | '|' { PIPE }
  | "${" { DOLLAR_BRACE }
  | '\"' {
      let string_start = lexbuf.Lexing.lex_start_p in
      reset_string_buffer ();
      string lexbuf;
      lexbuf.Lexing.lex_start_p <- string_start;
      STRING (get_string ())
    }
  | id { ID (Lexing.lexeme lexbuf ) }
  | eof { EOF }
  | _ { failwith "unknown token" }

and string =
  parse
  | eof { failwith "Unterminated string"; }
  | '\"' { () }
  | _ { add_char (Lexing.lexeme_char lexbuf 0); string lexbuf }

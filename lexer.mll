{
  open Parser
}

let string = ['a'-'z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*
let var = ['A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
  | [' ' '\t' '\n']      { token lexbuf }
  | ":-"            { RULE }
  | ';'             { SEMICOLON }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '['             { L_SQR }
  | ']'             { R_SQR }
  | ','             { COMMA }
  | '.'             { PERIOD }
  | string  as s    { STRING (s) }
  | var   as s      { VAR (s) }
  | eof             { EOF }
  | _               { ERROR }
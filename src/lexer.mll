{
}

let whitespace = [' ' '\t']+
let cr = ['\n' '\r' '\r''\n']
let up_letter = ['A'-'Z']
let var = up_letter
let digit = ['0'-'9']
let number = digit+
let relop = '<' | '<' ('>' | '=') | '>' | '>' ('<' | '=') | '='
let string = [' ' ',' '\'' '_' ';' ':' '(' ')' '.' 'a'-'z' 'A'-'Z']

rule main =
  parse
  | eof { () }

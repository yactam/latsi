{
open Parser
exception Lexing_error of string

let keywords_table = Hashtbl.create 7;;
List.iter (fun (kwd, token) -> Hashtbl.add keywords_table kwd token) 
                                [("IMPRIME", IMPRIME);
                                 ("SI", SI);
                                 ("ALORS", ALORS);
                                 ("VAVERS", VAVERS);
                                 ("ENTREE", ENTREE);
                                 ("FIN", FIN);
                                 ("REM", REM);
                                 ("NL", NL);
                                 ("SOUSROUTINE", SOUSROUTINE);
                                 ("RETOURNE", RETOURNE)]

let next_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with pos_bol = lexbuf.Lexing.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let lexing_position_to_string lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.sprintf "Line %d, column %d" pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 - String.length (Lexing.lexeme lexbuf))

let syntax_error msg lexbuf =
  let err = Printf.sprintf "Syntax error at %s: %s '%s'" (lexing_position_to_string lexbuf) msg (Lexing.lexeme lexbuf) in
  raise (Lexing_error err) 
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
  | whitespace { main lexbuf }
  | var as c { VAR (c) }
  | up_letter+ as keyword { 
                            try Hashtbl.find keywords_table keyword 
                            with Not_found -> syntax_error "Unexpected token" lexbuf
                          }
  | "<>" | "><" { NOTEQ }
  | "<=" { LEQ }
  | "<" { LT }
  | ">=" { GEQ }
  | ">" { GT }
  | "=" { EQUALS }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | '"' { parse_string (Buffer.create 64) lexbuf }
  | cr { next_line lexbuf; CR }
  | eof { EOF }
  | number as i { NUM (int_of_string i) }
  | _ { syntax_error "Unexpected token" lexbuf }

and parse_string buf =
  parse
  | '"' { STRING (Buffer.contents buf) }
  | string { Buffer.add_string buf (Lexing.lexeme lexbuf); parse_string buf lexbuf }
  | eof { syntax_error "Unterminated string" lexbuf }
  | _ { syntax_error "Illegal string character: " lexbuf }

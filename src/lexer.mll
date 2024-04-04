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

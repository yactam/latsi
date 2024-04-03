%{

%}

%token EOF IMPRIME SI ALORS VAVERS ENTREE FIN REM NL NOTEQ LEQ LT GEQ GT EQUALS 
%token PLUS MINUS TIMES DIV LPAREN RPAREN COMMA CR
%token <string> STRING
%token <char> VAR
%token <int> NUM

%start <unit> prog

%%

prog:
  | EOF { () }
  ;

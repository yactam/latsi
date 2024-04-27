%{
open Ast
%}

%token EOF IMPRIME SI ALORS VAVERS ENTREE FIN REM NL NOTEQ LEQ LT GEQ GT EQUALS
%token PLUS MINUS TIMES DIV LPAREN RPAREN COMMA CR
%token <string> STRING
%token <char> VAR
%token <int> NUM

%left PLUS MINUS
%left TIMES DIV

%start <Ast.program> prog

%%

prog:
  | l = ligne* EOF { l }

ligne:
  | n = NUM i = instr CR { Line (n, i) }

instr:
  | IMPRIME expr_lst = expr_list { Print (expr_lst) }
  | SI e1 = expression op = relop e2 = expression ALORS i = instr { IfThen (e1, op, e2, i) }
  | VAVERS e = expression { Goto (e) }
  | ENTREE var_lst = var_list { Scan (var_lst) }
  | v = VAR EQUALS e = expression { Assign (v, e) }
  | FIN { End }
  | REM s = STRING { Comment (s) }
  | NL { NewLine }

expr_list:
  | exprs = separated_nonempty_list(COMMA, expr_or_string) { exprs }

expr_or_string:
  | s = STRING { String (s) }
  | e = expression { Expression (e) }

expression:
  | n = NUM { Num (n) }
  | v = VAR { Var (v) }
  | PLUS e = expression { UnaryOp (Plus, e) }
  | MINUS e = expression { UnaryOp (Minus, e) }
  | e1 = expression PLUS e2 = expression { BinaryOp (Add, e1, e2) }
  | e1 = expression MINUS e2 = expression { BinaryOp (Sub, e1, e2) }
  | e1 = expression TIMES e2 = expression { BinaryOp (Mult, e1, e2) }
  | e1 = expression DIV e2 = expression { BinaryOp (Div, e1, e2) }
  | LPAREN e = expression RPAREN { ParenExpr (e) }

relop:
  | NOTEQ { NotEq }
  | LT { LessThan }
  | LEQ { LessOrEqual }
  | GT { GreaterThan }
  | GEQ { GreaterOrEqual }
  | EQUALS { Equal }

var_list:
  | vars = separated_nonempty_list(COMMA, VAR) { vars }
type unary_operator =
  | Plus
  | Minus

type binary_operator =
  | Add
  | Sub
  | Mult
  | Div

type relop =
  | NotEq | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual | Equal

type expression =
  | Num of int
  | Var of char
  | UnaryOp of unary_operator * expression
  | BinaryOp of binary_operator * expression * expression
  | ParenExpr of expression

type expr = 
  | String of string
  | Expression of expression

type instruction =
  | Print of expr list
  | IfThen of expression * relop * expression * instruction
  | Goto of expression
  | Scan of char list
  | Assign of char * expression
  | Comment of string
  | End 
  | NewLine

type line =
  | Line of int * instruction

type program = line list
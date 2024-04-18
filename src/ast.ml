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

let rec expr_as_string = function
  | String s -> "\"" ^ s ^ "\""
  | Expression e -> expression_as_string e
and expression_as_string = function
  
  | Num n -> string_of_int n
  | Var v -> Char.escaped v
  | UnaryOp (op, e) -> (match op with
                        | Plus -> "+" ^ expression_as_string e
                        | Minus -> "-" ^ expression_as_string e)
  | BinaryOp (op, e1, e2) -> 
      let op_str = match op with
                   | Add -> "+"
                   | Sub -> "-"
                   | Mult -> "*"
                   | Div -> "/"
      in
      "(" ^ expression_as_string e1 ^ " " ^ op_str ^ " " ^ expression_as_string e2 ^ ")"
  | ParenExpr e -> "(" ^ expression_as_string e ^ ")"

let rec instruction_as_string = function
  | Print expr_list ->
      "Print [" ^ String.concat "; " (List.map expr_as_string expr_list) ^ "]"
  | IfThen (e1, op, e2, instr) ->
      "If " ^ expression_as_string e1 ^ " " ^ 
      (match op with
       | NotEq -> "<>"
       | LessThan -> "<"
       | LessOrEqual -> "<="
       | GreaterThan -> ">"
       | GreaterOrEqual -> ">="
       | Equal -> "=") ^ " " ^ expression_as_string e2 ^ 
      " Then " ^ instruction_as_string instr
  | Goto e -> "Goto " ^ expression_as_string e
  | Scan char_list ->
      "Scan [" ^ String.concat ", " (List.map (fun c -> Char.escaped c) char_list) ^ "]"
  | Assign (var, e) -> "Assign " ^ Char.escaped var ^ " = " ^ expression_as_string e
  | Comment s -> "Comment \"" ^ s ^ "\""
  | End -> "End"
  | NewLine -> "NewLine"

let line_as_string = function
  | Line (n, instr) -> "Line " ^ string_of_int n ^ ": " ^ instruction_as_string instr

let program_as_string prog =
  String.concat "\n" (List.map line_as_string prog)

(************************************************************************************)
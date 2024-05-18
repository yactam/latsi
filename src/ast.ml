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

module Env = Map.Make(Char)
let env = ref (Env.empty : int Env.t);;

exception Interpretation_error of string

let rec eval_expression env = function
  | Num n -> n
  | Var v -> (try Env.find v !env with Not_found -> 
                                  raise (Interpretation_error ("Undefined variable: " ^ Char.escaped v)))
  | UnaryOp (op, e) ->
      let value = eval_expression env e in
      (match op with
       | Plus -> value
       | Minus -> -value)
  | BinaryOp (op, e1, e2) ->
      let value1 = eval_expression env e1 in
      let value2 = eval_expression env e2 in
      (match op with
       | Add -> value1 + value2
       | Sub -> value1 - value2
       | Mult -> value1 * value2
       | Div -> 
           if value2 = 0 then
             raise (Interpretation_error "Division by zero")
           else
             value1 / value2)
  | ParenExpr e -> eval_expression env e

let rec find_smallest_greater : line list -> int -> line  = fun sorted_list n ->
  match sorted_list with
  | []      -> failwith ""
  | l :: xs ->
    let Line (x, _) = l in
    if x > n then l else find_smallest_greater xs n

let rec eval_expr env = function
  | String string -> string
  | Expression expression -> string_of_int (eval_expression env expression)
and eval_instruction num instr lines env =
  match instr with
  | Print expr_list ->
      List.iter (fun expr -> print_string (eval_expr env expr)) expr_list;
      (try
        let next_line = find_smallest_greater lines num in
        eval_line next_line lines env;
      with
        | Failure _ -> ())
  | IfThen (e1, op, e2, instr) ->
      let value1 = eval_expression env e1 in
      let value2 = eval_expression env e2 in
      let result =
        match op with
        | NotEq -> value1 != value2
        | LessThan -> value1 < value2
        | LessOrEqual -> value1 <= value2
        | GreaterThan -> value1 > value2
        | GreaterOrEqual -> value1 >= value2
        | Equal -> value1 = value2
      in
      if result then eval_instruction num instr lines env 
      else 
      (try
        let next_line = find_smallest_greater lines num in
        eval_line next_line lines env;
      with
        | Failure _ -> ())
  | Goto e ->
    (let target = eval_expression env e in
    let next_line = List.find_opt (fun (Line(n, _)) -> n = target) lines in
    match next_line with
    | None   -> failwith "Goto non-existing line"
    | Some v -> eval_line v lines env)
  | Scan char_list ->
      List.iter (fun var -> let value = read_int () in env := Env.add var value !env) char_list;
      (try
        let next_line = find_smallest_greater lines num in
        eval_line next_line lines env;
      with
        | Failure _ -> ())
  | Assign (var, e) ->
      let value = eval_expression env e in
      env := Env.add var value !env;
      (try
        let next_line = find_smallest_greater lines num in
        eval_line next_line lines env;
      with
        | Failure _ -> ())
  | Comment _ -> 
    (try
      let next_line = find_smallest_greater lines num in
      eval_line next_line lines env;
    with
      | Failure _ -> ())
  | End -> ()
  | NewLine ->
    print_newline ();
    (try
        let next_line = find_smallest_greater lines num in
        eval_line next_line lines env;
      with
        | Failure _ -> ())

and eval_line : line -> line list -> int Env.t ref -> unit = fun line lines env ->
  let Line(num, instr) = line in
  eval_instruction num instr lines env

let initial_env env =
  let char_range = List.init 26 (fun i -> Char.chr (Char.code 'A' + i)) in
  List.iter (fun c -> env := Env.add c 0 !env) char_range

let eval_program program =
  initial_env env;
  let without_comment = List.filter (fun (Line(_, instr)) -> match instr with | Comment _ -> false | _ -> true ) program in
  let lines = List.sort (fun (Line(n1, _)) (Line(n2, _)) -> compare n1 n2) without_comment in
  match lines with
  | [] -> raise (Failure "Empty program")
  | first_line::_ ->
      eval_line first_line lines env
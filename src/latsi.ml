let () = 
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let channel = open_in filename in
    let lexbuf = Lexing.from_channel channel in	
    try
      let ast = Parser.prog Lexer.main lexbuf in
      let _ = Printf.printf "Parse:\n%s\n" (Ast.program_as_string ast) in
      let _ = Printf.printf "\nInterpretation:\n"; Ast.eval_program ast in 
      close_in channel;
    with
    | Lexer.Lexing_error msg ->
      Printf.eprintf "Lexing error: %s\n" msg;
      close_in channel;
    | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Syntax error at line %d, position %d\n"
        pos.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 - String.length (Lexing.lexeme lexbuf));
      close_in channel;
    | Failure msg ->
      close_in channel;
      Printf.eprintf "Error %s\n" msg;
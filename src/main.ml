open Lang

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


let run (f : string) = 
  match parse (In_channel.with_open_text f In_channel.input_all) with
  | exception Sys_error msg -> print_endline ("ERROR (file): " ^ msg)
  | exception Failure _ -> print_endline ("ERROR (lexer): unrecognized char.")
  | exception Parser.Error -> print_endline ("ERROR (parser): invalid syntax.")
  | e -> match type_of_expr (empty_context, e) with
    | None -> print_endline ("ERROR (type): the program is not well typed.")
    | Some _ -> print_endline (string_of_expr(multistep e))
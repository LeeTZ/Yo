open Ast

let _ =
	let lexbuf = Lexing.from_channel stdin in
	print_string (Scanner.token lexbuf)
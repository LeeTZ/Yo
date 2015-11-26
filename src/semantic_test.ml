
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.global Scanner.token lexbuf in
	let context = Built_in_type.generate_built_in_types in 
	let cc = Type_reader.walk_dec program context in
	Print_typetab.printtypetab cc.typetab
	
	(*let listing = Ast.string_of_program program
	in print_string listing*)
	
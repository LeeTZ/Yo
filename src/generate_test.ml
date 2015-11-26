
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.global Scanner.token lexbuf in
	let builtin = Built_in_type.generate_built_in_types in 
	let context = Type_reader.walk_dec program builtin in
	let seman = Semantic.build_semantic context program in
	let ccode = Generate.generate seman in 
	print_string ccode

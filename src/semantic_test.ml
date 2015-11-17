
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.global Scanner.token lexbuf in
	let context = Built_in_type.generate_built_in_types in 
	let seman = Semantic.build_semantic context program in
	let listing = Sast.string_of_s_program seman
	in print_string listing

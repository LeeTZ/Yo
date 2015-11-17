
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.global Scanner.token lexbuf in
	let typetab = Typereader.walk_dec program in 
	let seman = Semantic.build_semantic typetab program in
	let listing = Sast.string_of_s_program program
	in print_string listing

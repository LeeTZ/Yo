open Ast
let yoheader = "yoheader.yo"

let _ =
	let lexbuf = Lexing.from_channel (open_in yoheader) in	
	let program = Parser.global Scanner.token lexbuf in
	let builtincontext = Type_reader.walk_dec program {vsymtab=[NameMap.empty]; typetab=NameMap.empty} in
	let lexbuf = Lexing.from_channel stdin in	
	let program = Parser.global Scanner.token lexbuf in
	let context = Type_reader.walk_dec program builtincontext in
	(*Print_typetab.printtypetab context.typetab*)
	let seman = Semantic.build_semantic context program in
	let ccode = Generate.generate context seman in
	print_string ccode
(*
let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Parser.global Scanner.token lexbuf in
	let builtin = Built_in_type.generate_built_in_types in 
	let context = Type_reader.walk_dec program builtin in
	let seman = Semantic.build_semantic context program in
	let ccode = Generate.generate seman in 
	Print_typetab.printtypetab context.typetab*)

open Ast
let yoheader = "yoheader.yo"

let _ =
	let lexbuf = Lexing.from_channel (open_in yoheader) in	
	let program = Parser.global Scanner.token lexbuf in
	let builtincontext = Type_reader.walk_dec program {vsymtab=[NameMap.empty]; typetab=NameMap.empty} in
	let lexbuf = Lexing.from_channel stdin in	
	let program = Parser.global Scanner.token lexbuf in
	let context = Type_reader.walk_dec program builtincontext in
	Print_typetab.printtypetab context.typetab
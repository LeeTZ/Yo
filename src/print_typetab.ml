open Ast

let print_kv k v =
	print_string(k ^ "\n")

let printtypetab typetab = 
	NameMap.iter print_kv typetab

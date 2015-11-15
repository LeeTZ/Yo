let rec generate = function
  SLiteral (x, s) -> 
	if s.type_def.name = "String" then "\"" ^ x "\"" 
	else x
| SArrayLiteral -> "" (* defined later *)
| SVar (x, s) -> x
| SArrayExpr (x, y, s) -> generate x ^ "[" ^ generate y ^ "]"
| SDotExpr (x, y, s) -> generate x ^ "." ^ y 
| SBinop (x, op, y, s) -> generate x ^ s.type_def.actual ^ generate y
| SCall (x, y, z, s) -> 
	let rec generate_list = function
	  [] -> ""
	| hd -> generate hd
    | hd::tl -> generate hd ^ ", " ^ generate_list tl
	in
		if x = None then y ^ "(" ^ generate_list z ^ ")"
		else generate x ^ y ^ "(" ^ generate_list z ^ ")" 

| SAssign (x, s) -> 
	(match x with
	| None -> generate s ^ ";\n"
	| Some(x1) -> 
	(try List.find NewVar s.actions; 
		(match s.type_def.name with 
		| "Int" -> "int " ^ generate x1 ^ " = " ^ generate s ^ ";\n"
		| "Double" -> "double" ^ generate x1 ^ " = " ^ generate s ^ ";\n"
		| "String" -> "String" ^ generate x1 ^ " = " ^ generate s ^ ";\n"
		| "Bool" -> "bool" ^ generate x1 ^ " = " ^ generate s ^ ";\n"
		| _ -> let type_name = s.type_def.actual in
			"std::shared_ptr<" ^  type_name ^ "> " ^ generate x ^ " = std::make_shared<" ^ type_name^ ">;\n"
		)
	with Not_found -> generate x1 ^ "=" ^ generate s ^ ";\n"))

| SIfStmt (l) -> ""
| SForIn (x, l) -> ""
| SForEq (x, y ,l) -> ""
| SWhileStmt (x, l) -> ""
| SContinue -> "continue;\n"
| SBreak -> "break;\n"
| SReturn (x) -> 
	(match x with
	| None -> "return;\n"
	| Some(x1) -> "return " ^ generate x1 ^ ";")


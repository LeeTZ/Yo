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
	(match x with
	| None -> y ^ "(DUMMY_SELF, " ^ generate_list z ^ ")"
	| Some(x1) -> s.type_def.actual ^ "::" ^ y ^ "(" ^ generate x1 ^ ", " ^ generate_list z ^ ")" 

| SAssign (x, s) -> 
	(match x with
	| None -> generate s
	| Some(x1) -> 
	(try List.find NewVar x1.actions; 
		(match x1.type_def.name with 
		| "Int" -> "int " ^ generate x1 ^ " = " ^ generate s
		| "Double" -> "double" ^ generate x1 ^ " = " ^ generate s
		| "String" -> "String" ^ generate x1 ^ " = " ^ generate s
		| "Bool" -> "bool" ^ generate x1 ^ " = " ^ generate s
		| _ -> let type_name = x1.type_def.actual in
			"std::shared_ptr<" ^  type_name ^ "> " ^ generate x1 ^ " = std::make_shared<" ^ type_name^ ">()"
		)
	with Not_found -> (generate x1) ^ " = " ^ (generate s))) ^ ";\n"

| SCondExec (x, l) -> 
	let rec generate_list = function
	  [] -> ""
	| hd -> generate hd
    | hd::tl -> generate hd ^ generate_list tl
    in
	(match x with
	| None -> "else if (true) {\n" ^ generate_list l ^ "}"
	| Some(x1) -> "else if (" ^ generate x1 ^ ") {\n" ^ generate_list l ^ "}"
	)
| SIfStmt (l) -> "if(false) {}\n" ^ 
	(let rec generate_list = function
	  [] -> ""
	| hd -> generate hd
    | hd::tl -> generate hd ^ generate_list tl
    in generate_list l)
| SForIn (x, l) -> ""
| SForEq (x, y ,l) -> ""
| SWhileStmt (x, l) -> "while (" ^ generate x ")" ^
	(let rec generate_list = function
	  [] -> ""
	| hd -> generate hd
    | hd::tl -> generate hd ^ generate_list tl
    in generate_list l)
| SContinue -> "continue;\n"
| SBreak -> "break;\n"
| SReturn (x) -> "return " ^
	(match x with
	| None -> ""
	| Some(x1) -> generate x1) ^ ";\n"


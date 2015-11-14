open Ast
open Common

let compile_type context program = 
	(* lookup a variable from local to global in vsymtab. Usage: look_up_var id context.vsymtab *)
	let rec look_up_var id = function 
		| [] -> raise (VariableNotDefined id)
		| hd :: tail -> (try NameMap.find id hd with Not_found -> look_up_var id tail)
		in
	let rec look_up_type typeName typenv = (try NameMap.find typeName typenv 
			with Not_found -> raise (TypeNotDefined typeName))
		in
		
	
	(* resolve type for an expression *)
	let rec resolve_type ctx = function
		| IntConst -> look_up_type "Int" ctx.typetab
		| DoubleConst -> look_up_type "Double" ctx.typetab
		| BoolConst -> look_up_type "Bool" ctx.typetab
		| StrConst -> look_up_type "String" ctx.typetab
		| ArrayConst | ArrayExpr -> look_up_type "Array" ctx.typetab
		| Var x -> (look_up_var x ctx.vsymtab).type_def
		| DotExpr (expr, x) -> (try NameMap.find x (resolve_type ctx expr).members
			with Not_found -> raise (VariableNotDefined (	x ^ " in " ^ (string_of_expr expr))))
		in	
		
	let rec build_semantic ctx = function
		| IntConst x -> SLiteral (x, {actions=[]; type_def=look_up_type "Int" ctx.typetab})
		| DoubleConst x -> SLiteral (x, {actions=[]; type_def=look_up_type "Float" ctx.typetab})
		| BoolConst x -> SLiteral (x, {actions=[]; type_def=look_up_type "Bool" ctx.typetab})
		| StrConst x -> SLiteral (x, {actions=[]; type_def=look_up_type "String" ctx.typetab})
		| ArrayConst x -> SArrayLiteral ((List.iter (build_semantic ctx) x), {actions=[]; type_def=look_up_type "Array" ctx.typetab})
		| Var x -> SVar (x, {actions=[]; type_def = (look_up_var x ctx.vsymtab).type_def})
		| DotExpr (expr, x) -> let sexpr = build_semantic ctx expr in
														SDotExpr (sexpr, x,  (try NameMap.find x (extract_semantic sexpr).type_def.members 
																		with Not_found -> raise (VariableNotDefined (	x ^ " in " ^ (string_of_expr expr)))))
		| Binop (x, op, y) -> let b1 = build_semantic ctx x and b2 = build_semantic ctx y in
														let s1=extract_semantic b1 and s2=extract_semantic b2 in
														if s1.type_def = s2.type_def then SBinop (b1, op, b2, s1) 
														else raise (SemanticError (	(string_of_expr x) ^ " and " ^ (string_of_expr y) ^ " must be of the same type"))
		| Call (obj, name, args) -> let s_call_args = List.iter (fun e -> build_semantic e) args in (*build semantics for args*)
					let upper_name = String.uppercase name in 
					let func_name = match obj with (*should we change a name when searching for the function?*)
					| None -> upper_name
					| Some -> let sobj = build_semantic obj in 
									let t = (extract_semantic sobj).type_def in
									(String.uppercase t) ^ "." ^ upper_name in
					let func_type = try NameMap.find func_name ctx.typetab (* get type_entry for this func *)
													with Not_found -> raise (TypeNotDefined ("Function " ^ name ^ " is not defined")) in
					let func_eval = try List.find (fun e -> e.args = args) func_type.evals  (* get the matching eval *)
												with Not_found -> raise (TypeNotDefined ("Function " ^ name ^ " does not take params of type " 
																																		(List.iter string_of_expr args))) in
					SCall ((build_semantic obj), func_name, s_call_args, Some(func_eval.type_entry))
					
					
		in
	(* create a new variable for the given name and type in the topmost variable map in vsymtab *)	
	let new_var ctx varName typeName =
		let actual_type = look_up_type typeName ctx.typetab in
		ctx.vsymtab <-  (NameMap.add varName 
			{name=varName; actual=varName ^ "_"; type_def=actual_type} 
			(List.hd ctx.vsymtab)) :: (List.tl ctx.vsymtab); actual_type
		in
		
		
	let compile_stmt ctx = function
		| Assign (e1, e2) -> 
			let rType = (resolve_type ctx e2) in
			let lType = (try resolve_type ctx e1 with VariableNotDefined -> match e1 with 
				| Var x -> new_var ctx x rType.name
				| _ -> raise (SemanticError "Invalid assignment: lvalue " ^ (string_of_expr e1) ^ " not found")) in
		if lType.name = rType.name then [] 
		else raise (SemanticError "Invalid assignment: expecting " ^ lType.name ^ ", but having " ^ rType.name )
	resolve_type context program
	

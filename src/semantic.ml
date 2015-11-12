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
		| Id x -> (look_up_var x ctx.vsymtab).type_def
		| DotExpr (expr, x) -> (try NameMap.find x (resolve_type ctx expr).members
			with Not_found -> raise (VariableNotDefined (	x ^ " in " ^ (string_of_expr expr))))
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
				| Id x -> new_var ctx x rType.name
				| _ -> raise (SemanticError "Invalid assignment: lvalue " ^ (string_of_expr e1) ^ " not found")) in
		if lType.name = rType.name then [] 
		else raise (SemanticError "Invalid assignment: expecting " ^ lType.name ^ ", but having " ^ rType.name )
	resolve_type context program
	

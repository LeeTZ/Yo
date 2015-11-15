open Ast
open Common

  let build_semantic context program = 
  	(* lookup a variable from local to global in vsymtab. Usage: look_up_var id context.vsymtab *)
  	let rec look_up_var id = function 
  		| [] -> raise (VariableNotDefined id)
  		| hd :: tail -> (try NameMap.find id hd with Not_found -> look_up_var id tail)
  		in
  	
  	let rec look_up_type typeName typenv = (try NameMap.find typeName typenv 
  			with Not_found -> raise (TypeNotDefined typeName))
  		in	
  		(*
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
  		*)
  	let rec build_expr_semantic ctx = function
  		| IntConst x -> 		SLiteral (string_of_int x, 
  																	{actions=[]; type_def=look_up_type "Int" ctx.typetab})
  		| DoubleConst x -> 	SLiteral (string_of_float x, 
  																	{actions=[]; type_def=look_up_type "Float" ctx.typetab})
  		| BoolConst x -> 		SLiteral (string_of_bool x, 
  																	{actions=[]; type_def=look_up_type "Bool" ctx.typetab})
  		| StrConst x -> 		SLiteral (x, 
  																	{actions=[]; type_def=look_up_type "String" ctx.typetab})
  		
  		| ArrayConst x -> 	SArrayLiteral ((List.map (build_expr_semantic ctx) x), 
  																	{actions=[]; type_def=look_up_type "Array" ctx.typetab})
  		
  		| Var x -> 					SVar (x, 
  																	{actions=[]; type_def = (look_up_var x ctx.vsymtab).type_def})
  		
  		| DotExpr (expr, x) -> let sexpr = build_expr_semantic ctx expr in
  														SDotExpr (sexpr, x,  (try 
  																	{actions=[]; type_def=NameMap.find x (extract_semantic sexpr).type_def.members} 
  																		with Not_found -> raise (VariableNotDefined (	x ^ " in " ^ (string_of_expr expr)))))
  		
  		| Binop (x, op, y) -> let b1 = build_expr_semantic ctx x and b2 = build_expr_semantic ctx y in (* TODO: more type checking *)
  														let s1=extract_semantic b1 and s2=extract_semantic b2 in
  														if s1.type_def = s2.type_def then SBinop (b1, op, b2, {actions=[]; type_def=s1.type_def}) 
  														else raise (SemanticError ((string_of_expr x) ^ " and " ^ (string_of_expr y) ^ " must be of the same type"))
  		
  		| Call (obj, name, args) -> (let s_call_args = List.map (fun e -> build_expr_semantic ctx e) args in (*build semantics for args*)
  					let upper_name = String.uppercase name in 
  					let func_name = match obj with (*should we change a name when searching for the function?*)
  					| None -> upper_name
  					| Some x -> let sobj = build_expr_semantic ctx x in 
  									let t = (extract_semantic sobj).type_def in
  									(String.uppercase t.name) ^ "." ^ upper_name in
  					let func_type = try NameMap.find func_name ctx.typetab (* get type_entry for this func *)
  													with Not_found -> raise (TypeNotDefined ("Function " ^ name ^ " is not defined")) in
  					let call_arg_types = List.map (fun e -> (extract_semantic e).type_def) s_call_args in
  					let func_eval = try List.find (fun e -> (List.map (fun (k: var_entry) -> k.type_def) e.args) = call_arg_types) func_type.evals  (* get the matching eval *)
  												with Not_found -> raise (TypeNotDefined ("Function " ^ name ^ " does not take params of type " ^ (String.concat ", " (List.map string_of_expr args)))) in
  					SCall ((match obj with 
  					 				| None -> None 
  									| Some s -> Some(build_expr_semantic ctx s)), 
  								func_name, s_call_args, 
  								{actions=[]; type_def=(match func_eval.ret with 
  															| None -> look_up_type "Void" ctx.typetab
  															| Some x -> x)}))
  					
  	in
  	(* create a new variable for the given name and type in the topmost variable map in vsymtab *)	
  	let new_var ctx varName typeDef =
  		ctx.vsymtab <- (NameMap.add varName 
  			{name=varName; actual=varName ^ "_"; type_def=typeDef} 
  			(List.hd ctx.vsymtab)) :: (List.tl ctx.vsymtab); SVar (varName, {actions=[NewVar]; type_def=typeDef})
  		in
  		
  	let rec build_stmt_semantic ctx = function
  		| Assign (e1, e2) -> 
  			(let r_expr_sem = (build_expr_semantic ctx e2) in
  			match e1 with
  				| None -> SAssign (None, r_expr_sem)
  				| Some e -> try let l_expr_sem = build_expr_semantic ctx e in
  							let ltype = (extract_semantic l_expr_sem).type_def and rtype = (extract_semantic r_expr_sem).type_def in
  							if  ltype = rtype then SAssign (Some(l_expr_sem), r_expr_sem) else raise (SemanticError ("Invalid assignment: expecting " ^ ltype.name ^ ", but having " ^ rtype.name ))
  					with VariableNotDefined s -> match e with 
  					| Var x -> let l_expr_sem = new_var ctx x (extract_semantic r_expr_sem).type_def in SAssign (Some(l_expr_sem), r_expr_sem)
  					| _ -> raise (SemanticError ("Invalid assignment: lvalue " ^ (string_of_expr e) ^ " not found")))
  		| IfStmt cl -> SIfStmt (List.map 
  					(fun t -> match t with CondExec (x,y) -> let s_stmt_list = List.map (build_stmt_semantic ctx) y in
  						match x with 
  						| None -> SCondExec(None,  s_stmt_list)
  						| Some cond -> let expr_sem = build_expr_semantic ctx cond in
  											if (extract_semantic expr_sem).type_def.name = "Bool" then SCondExec(Some(expr_sem), s_stmt_list)
  											else raise (SemanticError ("Condition expression" ^ (string_of_expr cond) ^ " in the if statement should be of Bool type")) )
  					cl)
  		| Continue -> SContinue
  		| Break -> SBreak
  		| Return exp -> match exp with 
  			| None -> SReturn (None)
  			| Some x -> SReturn (Some(build_expr_semantic ctx x))
  	
  	in
  	build_stmt_semantic context program
	

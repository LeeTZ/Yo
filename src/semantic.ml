(*TODO : break continue in if statement? 
          in while statement?
*)


open Ast
open Sast

let rec look_up_var id = function 
  		| [] -> raise (VariableNotDefined id)
  		| hd :: tail -> (try NameMap.find id hd with Not_found -> look_up_var id tail)

(* lookup a variable from local to global in vsymtab. Usage: look_up_var id context.vsymtab *)
let rec look_up_type typeName typenv = (try NameMap.find typeName typenv 
		with Not_found -> raise (TypeNotDefined typeName))

(* create a new variable for the given name and type in the topmost variable map in vsymtab *)	
let new_var ctx varName typeDef =
	try NameMap.find varName (List.hd ctx.vsymtab); raise (SemanticError (varName ^ " already defined in current scope"))
	with Not_found -> ctx.vsymtab <- (NameMap.add varName 
		{name=varName; actual=varName ^ "_"; type_def=typeDef} 
		(List.hd ctx.vsymtab)) :: (List.tl ctx.vsymtab); SVar (varName, {actions=[NewVar]; type_def=typeDef})

let push_var_env ctx = {ctx with vsymtab = NameMap.empty :: ctx.vsymtab}
  	
let find_matching_eval func_type call_arg_types = 
	List.find (fun e -> (List.map (fun (k: var_entry) -> k.type_def) e.args) = call_arg_types) func_type.evals		

let extract_arr_ele_type ctx (arr_type: type_entry) = 
	let exp_ele_tn_len = (String.length arr_type.name)-2 in
	let arr_ele_name = if (String.sub arr_type.name exp_ele_tn_len 2) = "[]" 
										then String.sub arr_type.name 0 exp_ele_tn_len else raise (Invalid_argument (arr_type.name ^ " is not of Array type")) in
	look_up_type arr_ele_name ctx.typetab

(* build semantic for an expression; extract_semantic can be usd to get the semantic*)
let rec build_expr_semantic ctx = function
			(* Int, Double, Bool, Str are consolidated into SLiteral since there aren't much*)
			(* difference in code generation: just print the string representation! *)
  		| IntConst x -> 		SLiteral (string_of_int x, 
  																	{actions=[]; type_def=look_up_type "Int" ctx.typetab})
  		| DoubleConst x -> 	SLiteral (string_of_float x, 
  																	{actions=[]; type_def=look_up_type "Double" ctx.typetab})
  		| BoolConst x -> 		SLiteral (string_of_bool x, 
  																	{actions=[]; type_def=look_up_type "Bool" ctx.typetab})
  		| StrConst x -> 		SLiteral (x, 
  																	{actions=[]; type_def=look_up_type "String" ctx.typetab})
  		
  		| ArrayConst x -> let arrayType = match x with (* determine the array type by its first element *)
												| [] -> raise (SemanticError ("Array literal length has to be at least 1"))
												| hd::tl -> (extract_semantic (build_expr_semantic ctx hd)).type_def in
												let arraySem = {actions=[NewVar]; type_def=look_up_type (arrayType.name^"[]") ctx.typetab} in
					 							SArrayLiteral (
														(List.map (fun e -> let s = build_expr_semantic ctx e in 
														if (extract_semantic s).type_def = arrayType then s 
														else raise (SemanticError ("Array literal length has to be at least 1"))) x), arraySem)
  		(* try to find the variable in the symbol table; may throw exception when it is not found *)
  		| Var x -> 					SVar (x, 
  																	{actions=[]; type_def = (look_up_var x ctx.vsymtab).type_def})
  		| NewArray s ->				SNewArray({actions=[NewArr]; type_def = look_up_type (s^"[]") ctx.typetab})
			
  		| DotExpr (expr, x) -> let sexpr = build_expr_semantic ctx expr in
  														SDotExpr (sexpr, x,  (try 
  																	{actions=[]; type_def=NameMap.find x (extract_semantic sexpr).type_def.members} 
  																		with Not_found -> raise (VariableNotDefined (	x ^ " in " ^ (string_of_expr expr)))))
  		
  		| Binop (x, op, y) -> let b1 = build_expr_semantic ctx x and b2 = build_expr_semantic ctx y in (* TODO: more type checking *)
  														let s1=extract_semantic b1 and s2=extract_semantic b2 in
															let call_arg_types = [s1.type_def; s2.type_def] in
															let func_eval = try find_matching_eval (look_up_type (binop_type_tab op) ctx.typetab) [s1.type_def; s2.type_def]  (* get the matching eval *)
  																				with Not_found -> raise (SemanticError ("Operator " ^ (string_of_op op) 
																					^ " does not take params of type " ^ s1.type_def.name ^ " and " ^ s2.type_def.name)) 
															in SBinop (b1, op, b2, {actions=[]; type_def=func_eval.ret})
  														  		
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
  					let func_eval = try find_matching_eval func_type call_arg_types (* get the matching eval *)
  												with Not_found -> raise (TypeNotDefined ("Function " ^ name ^ " does not take params of type " ^ (String.concat ", " (List.map string_of_expr args)))) in
  					SCall ((match obj with 
  					 				| None -> None 
  									| Some s -> Some(build_expr_semantic ctx s)), 
  								func_name, s_call_args, 
  								{actions=[]; type_def=func_eval.ret}))

let rec build_stmt_semantic ctx = function
			
  		| Assign (e1, e2) -> 
  			(let r_expr_sem = (build_expr_semantic ctx e2) in
  			match e1 with
					(* When there is no l-value, set the semantic of the stmt to that of r-value *)
  				| None -> SAssign (None, r_expr_sem)
  				| Some e -> try let l_expr_sem = build_expr_semantic ctx e in
								(* When the l-value is defined, check its type against that of r-value *)
  							let ltype = (extract_semantic l_expr_sem).type_def and rtype = (extract_semantic r_expr_sem).type_def in
  							if  ltype = rtype then SAssign (Some(l_expr_sem), r_expr_sem) else raise (SemanticError ("Invalid assignment: expecting " ^ ltype.name ^ ", but having " ^ rtype.name ))
  					(* create a new variable with the type of r-value *)
						with VariableNotDefined s -> match e with 
  					| Var x -> let l_expr_sem = new_var ctx x (extract_semantic r_expr_sem).type_def in SAssign (Some(l_expr_sem), r_expr_sem)
  					(* make sure l-value is a var: otherwise it must be defined (processed above) *)
						| _ -> raise (SemanticError ("Invalid assignment: lvalue " ^ (string_of_expr e) ^ " not found")))
  		| IfStmt cl -> let ctx2 = push_var_env ctx in
						SIfStmt (List.map (* go through each cond_exec *)
  					(fun t -> match t with CondExec (x,y) -> let s_stmt_list = List.map (build_stmt_semantic ctx2) y in
  						match x with 
  						| None -> SCondExec(None,  s_stmt_list) (* the final else has no predicate *)
  						| Some cond -> let expr_sem = build_expr_semantic ctx2 cond in (* make sure the predicate has type Bool *)
  											if (extract_semantic expr_sem).type_def.name = "Bool" then SCondExec(Some(expr_sem), s_stmt_list)
  											else raise (SemanticError ("Condition expression" ^ (string_of_expr cond) ^ " in the if statement should be of Bool type")) )
  					cl)
			| WhileStmt (pred, stmts) -> let ctx2 = push_var_env ctx in
					let s_pred = build_expr_semantic ctx2 pred in
					if (extract_semantic s_pred).type_def.name = "Bool" then () 
					else raise (SemanticError ("Condition expression" ^ (string_of_expr pred) ^ " in the while statement should be of Bool type"));
					SWhileStmt (s_pred, List.map (build_stmt_semantic ctx2) stmts)
					
  		| Continue -> SContinue
  		| Break -> SBreak
  		| Return exp -> (match exp with 
  			| None -> SReturn (None)
  			| Some x -> SReturn (Some(build_expr_semantic ctx x)))
			| ForIn (var, expr, stmts) -> let s_expr = build_expr_semantic ctx expr in
					let arr_ele_type = try extract_arr_ele_type ctx (extract_semantic s_expr).type_def 
															with Invalid_argument _ -> raise (SemanticError ((string_of_expr expr)^" in the for loop has to be of Array type")) in
					let nested_env = push_var_env ctx in
					let svar = new_var nested_env var arr_ele_type in
					let s_stmt_list = List.map (build_stmt_semantic ctx) stmts in
					SForIn(var, (extract_semantic svar), s_expr, s_stmt_list)
																		

let build_func_semantic ctx = function (* TODO: cyclic reference *)
	FuncDecl (funcName, argList, stmtList) -> 
		let ctx2 = push_var_env ctx in (* create a new variable env on top of the old *)
		let sarglist = List.map (*  *)
				(fun x -> match x with VarDecl (name, typename) ->
					let svar = new_var ctx2 name (look_up_type typename ctx.typetab) in
					SVarDecl (name, extract_semantic svar)) argList in
		let s_stmtlist = List.map (build_stmt_semantic ctx2) stmtList in
		let ret_types =
			let add_to_ret_types lst = function 
				| SReturn expr_option -> (match expr_option with 
															| Some ep ->  (extract_semantic ep).type_def :: lst
															| None -> (look_up_type "Void" ctx.typetab) :: lst)																
				| _ -> lst in					
			List.fold_left (fun rlst x -> match x with 
				| SReturn r -> add_to_ret_types rlst (SReturn r)
				| SIfStmt ceList -> List.fold_left add_to_ret_types rlst 
								(List.flatten (List.map (fun ce -> match ce with SCondExec (_, stList) -> stList) ceList))
				| SForIn (_, _, _, stList) -> List.fold_left add_to_ret_types rlst stList
				| SForEq (_, _, _, _, stList) -> List.fold_left add_to_ret_types rlst stList
				| _ -> rlst
			) [] s_stmtlist	
		in
    if (List.length ret_types) < 1 then raise (SemanticError ("Function " ^ funcName ^ " has to have a return statement")) else ();
		try (List.find (fun x -> x <> (List.hd ret_types))  ret_types; raise (SemanticError ("All return statements should return the same type in " ^ funcName))) 
		with Not_found -> SFuncDecl (funcName, sarglist, s_stmtlist, {actions=[]; type_def=List.hd ret_types})

let rec build_type_mem_semantic ctx = function
			| MemFuncDecl memfunc -> SMemFuncDecl (build_func_semantic ctx memfunc) 
			| MemTypeDecl memtype -> SMemTypeDecl (build_type_semantic ctx memtype)
			| MemVarDecl 	memvar 	-> match memvar with VarDecl (varname, vartype) -> SMemVarDecl (SVarDecl (varname, {actions=[]; type_def=look_up_type vartype ctx.typetab}))
			
and build_type_semantic ctx = function
			| TypeDecl (typename, memlist) -> STypeDecl (typename, List.map (build_type_mem_semantic ctx) memlist)
		
let build_program_semantic ctx = function
			| GlobalStmt stmt -> SGlobalStmt (build_stmt_semantic ctx stmt)
			| GlobalType type_decl -> SGlobalType (build_type_semantic ctx type_decl)
			| GlobalFunc func_decl -> SGlobalFunc (build_func_semantic ctx func_decl)


let build_semantic context program = List.map (build_program_semantic context) program
		


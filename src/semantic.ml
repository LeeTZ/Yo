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

(* lookup a variable from local to global in vsymtab. Usage: look_up_var id context.vsymtab *)
let rec look_up_type2 typeName typenv = 
	try 
	(let rec generate_simple_type_name = function
		| SimpleType s -> s
		| NestedType(m, s) -> (generate_simple_type_name m) ^ "_" ^ s 
		| _ -> raise (SemanticError "Nested type should be SimpleType") in
	let rec generate_type = function 
		| ArrayType t -> ArrayTypeEntry(generate_type t)
		| x -> BaseTypeEntry(NameMap.find (generate_simple_type_name x) typenv) in
	generate_type typeName)
	with Not_found -> raise (TypeNotDefined (string_of_type_name typeName))


(* create a new variable for the given name and type in the topmost variable map in vsymtab *)	
let new_var ctx varName typeDef =
	try let _ = NameMap.find varName (List.hd ctx.vsymtab) in 
		raise (SemanticError (varName ^ " already defined in current scope"))
	with Not_found -> ctx.vsymtab <- (NameMap.add varName 
		{v_name=varName; v_actual=varName ^ "_"; v_type=typeDef} 
		(List.hd ctx.vsymtab)) :: (List.tl ctx.vsymtab); SVar (varName, {actions=[NewVar]; type_def=typeDef})

let push_var_env ctx = {ctx with vsymtab = NameMap.empty :: ctx.vsymtab}

let rec string_of_arg = function | BaseTypeEntry e -> e.t_name | ArrayTypeEntry e -> "Array<" ^ (string_of_arg e) ^ ">"
  	
let find_matching_eval func_type call_arg_types =
	let rec compare_args args1 args2 = match args1, args2 with 
		| [], [] -> true | _, [] | [], _ -> false |  x::xs, y::ys -> (compare_type x y) && (compare_args xs ys) in
	List.find (fun e ->	compare_args (List.map (fun k -> k.v_type) e.args) call_arg_types) func_type.evals

(* build semantic for an expression; extract_semantic can be usd to get the semantic*)
let rec build_expr_semantic ctx (expression:expr) : s_expr=  
	let int_type = BaseTypeEntry(look_up_type "Int" ctx.typetab) 
	and double_type = BaseTypeEntry(look_up_type "Double" ctx.typetab)
	and string_type = BaseTypeEntry (look_up_type "String" ctx.typetab) in
	let bool_type = BaseTypeEntry(look_up_type "Bool" ctx.typetab) 
	and clip_type = BaseTypeEntry(look_up_type "Clip" ctx.typetab)
	and frame_type = BaseTypeEntry(look_up_type "Frame" ctx.typetab) in
	match expression with
	(* Int, Double, Bool, Str are consolidated into SLiteral since there aren't much*)
	(* difference in code generation - just print the string representation! *)
  	| IntConst x -> SLiteral(string_of_int x, {actions=[]; type_def=int_type})
	| DoubleConst x -> SLiteral(string_of_float x, {actions=[]; type_def=double_type})
	| BoolConst x -> SLiteral(string_of_bool x, {actions=[]; type_def=bool_type})
	| StrConst x -> SLiteral(x, {actions=[]; type_def=string_type})

  	| ArrayConst x -> 
  		let elementType = match x with (* determine the array type by its first element *)
			| [] -> raise (SemanticError ("Array literal length has to be at least 1"))
			| hd::tl -> (extract_semantic (build_expr_semantic ctx hd)).type_def in
		let arraySem = {actions=[NewArr]; type_def=ArrayTypeEntry(elementType)} in
		SArrayLiteral (
			(List.map (fun e -> let s = build_expr_semantic ctx e in 
			if (extract_semantic s).type_def = elementType then s 
			else raise (SemanticError ("Element types in the array has to be uniform"))) x), arraySem)
  	(* try to find the variable in the symbol table; may throw exception when it is not found *)
  	| Var x -> SVar (x, {actions=[]; type_def=(look_up_var x ctx.vsymtab).v_type})

	| ArrayIndex (main, idx) -> 
		let smain = build_expr_semantic ctx main and sidx = build_expr_semantic ctx idx in
		(let main_type = (extract_semantic smain).type_def in
			match main_type with 
			| BaseTypeEntry t -> (
				if compare_type main_type clip_type then 
					(if compare_type (extract_semantic sidx).type_def double_type
					then SClipTimeIndex(smain, sidx, {actions=[]; type_def=frame_type})
					else if compare_type (extract_semantic sidx).type_def int_type
					then SClipFrameIndex(smain, sidx, {actions=[]; type_def=frame_type})
					else raise (SemanticError ((string_of_expr idx) ^ " has to be of either Double or Int type")))
				else raise (SemanticError ((string_of_expr main) ^ " has to be of Array type")))
			| ArrayTypeEntry ele_type -> (
				if compare_type (extract_semantic sidx).type_def int_type
				then SArrayIndex(smain, sidx, {actions=[]; type_def=ele_type})
				else raise (SemanticError ((string_of_expr idx) ^ " has to be of Int type"))))
			
	| ArrayRange (main, st, ed) ->
		let smain = build_expr_semantic ctx main in
		let sst = build_expr_semantic ctx st and sed = build_expr_semantic ctx ed in 
		let type_main = (extract_semantic smain).type_def in
		(match type_main with 
			| BaseTypeEntry t -> (
				if compare_type type_main clip_type then 
					(if compare_type (extract_semantic sst).type_def double_type 
						&& compare_type (extract_semantic sed).type_def double_type
					then SClipTimeRange(smain, sst, sed, {actions=[]; type_def=clip_type})
					else if compare_type (extract_semantic sst).type_def int_type 
						&& compare_type (extract_semantic sed).type_def int_type
					then SClipFrameRange(smain, sst, sed, {actions=[]; type_def=clip_type})
					else raise (SemanticError ((string_of_expr st) ^ " and " ^ (string_of_expr ed) 
						^ " have to be of both Double types or Int types")))
				else raise (SemanticError ((string_of_expr main) ^ " has to be of Array type")))
			| ArrayTypeEntry _ -> (
				if compare_type (extract_semantic sst).type_def int_type 
					&& compare_type (extract_semantic sed).type_def int_type
				then SArrayRange(smain, sst, sed, {actions=[]; type_def=type_main})
				else raise (SemanticError ((string_of_expr st) ^ " and " ^ (string_of_expr ed) ^ " have to be of Int type"))))
	
  	| DotExpr (expr, x) -> 
  		let sexpr = build_expr_semantic ctx expr in
			SDotExpr (sexpr, x,  (try {actions=[]; type_def=NameMap.find x (
										match (extract_semantic sexpr).type_def with 
											| BaseTypeEntry t -> t.members
											| ArrayTypeEntry t -> (look_up_type "ARRAY" ctx.typetab).members)} 
								with Not_found -> raise (VariableNotDefined (x ^ " in " ^ (string_of_expr expr)))))
  		
  	| Binop (x, op, y) -> 
  		let b1 = build_expr_semantic ctx x and b2 = build_expr_semantic ctx y in
		let s1 = extract_semantic b1 and s2 = extract_semantic b2 in
		let func_eval = try find_matching_eval (look_up_type (binop_type_tab op) ctx.typetab) [s1.type_def; s2.type_def]
						with Not_found -> raise (SemanticError ("Operator " ^ (string_of_op op) 
							^ " does not take params of type " ^ (string_of_type s1.type_def) 
							^ " and " ^ (string_of_type s2.type_def))) 
		in SBinop (b1, op, b2, {actions=[]; type_def=func_eval.ret})
  	
  														  		
  	| Call (obj, fname, args) -> 	
  		let s_call_args = List.map (fun e -> build_expr_semantic ctx e) args in (*build semantics for args*)
		let func_name = match obj with (*should we change a name when searching for the function?*)
			| None -> fname
			| Some x -> let sobj = build_expr_semantic ctx x in 
						let t = (extract_semantic sobj).type_def in
						(string_of_type t) ^ "_" ^ fname in
		let func_type = try NameMap.find func_name ctx.typetab (* get type_entry for this func *)
						with Not_found -> raise (TypeNotDefined ("Function " ^ fname ^ " is not defined")) in
		let augmented_s_args = match obj with 
			| None -> s_call_args
			| Some x -> (build_expr_semantic ctx x) :: s_call_args in
		let call_arg_types = List.map (fun e -> (extract_semantic e).type_def) augmented_s_args in
		let func_eval = try find_matching_eval func_type call_arg_types (* get the matching eval *)
						with Not_found -> raise (TypeNotDefined ("Function " ^ fname ^ " does not take params of type " 
								^ (String.concat ", " (List.map string_of_expr args)))) in
		SCall ((match obj with 
		 				| None -> None 
						| Some s -> Some(build_expr_semantic ctx s)), 
					func_type, s_call_args, 
					{actions=[]; type_def=func_eval.ret})
	
	| ClipConcat (cl1, cl2, tm) ->
		let scl1 = build_expr_semantic ctx cl1 and scl2 = build_expr_semantic ctx cl2 and stm = build_expr_semantic ctx tm in
		if compare_type (extract_semantic scl1).type_def clip_type 
			&& compare_type (extract_semantic scl2).type_def clip_type 
			&& compare_type (extract_semantic stm).type_def double_type
		then SClipConcat(scl1, scl2, stm, {actions=[]; type_def=clip_type})
		else raise (SemanticError "ClipConcat operation expects type of Clip, Clip and Double")
	
	| BuildArray (main, args) ->
		let rec resolve_ele_type = function
			| SimpleArrayConstructor expr -> BaseTypeEntry(look_up_type (string_of_expr expr) ctx.typetab)
			| CompositeArrayConstructor arr -> ArrayTypeEntry(resolve_ele_type arr) in
		let ele_type = resolve_ele_type main in
		SBuildArray (ele_type, [], {actions=[NewArr]; type_def=ArrayTypeEntry(ele_type)}) (*TODO: the constructor argument is currently ignored*)

let rec build_stmt_semantic ctx = function
	| Assign (e1, e2) -> 
		(let r_expr_sem = (build_expr_semantic ctx e2) in
		match e1 with
			(* When there is no l-value, set the semantic of the stmt to that of r-value *)
			| None -> SAssign (None, r_expr_sem)
			| Some e -> 
				try let l_expr_sem = build_expr_semantic ctx e in
				(* When the l-value is defined, check its type against that of r-value *)
					let ltype = (extract_semantic l_expr_sem).type_def 
					and rtype = (extract_semantic r_expr_sem).type_def in
					if  compare_type ltype rtype 
						then SAssign (Some(l_expr_sem), r_expr_sem) 
						else raise (SemanticError ("Invalid assignment: expecting " ^ (string_of_type ltype) 
							^ ", but having " ^ (string_of_type rtype) ))
				(* create a new variable with the type of r-value *)
				with VariableNotDefined s -> match e with 
				| Var x -> let l_expr_sem = new_var ctx x (extract_semantic r_expr_sem).type_def in 
					SAssign (Some(l_expr_sem), r_expr_sem)
				(* make sure l-value is a var: otherwise it must be defined (processed above) *)
				| _ -> raise (SemanticError ("Invalid assignment: lvalue " ^ (string_of_expr e) ^ " not found")))
	| SetAttribute (main, time, value) ->
		let s_main = (match main with 
			| DotExpr (expr, x) -> build_expr_semantic ctx main
			| _ -> raise (SemanticError ((string_of_expr main) ^ "is expected to have DotExpr here")))
		and s_time = build_expr_semantic ctx time 
		and s_value = build_expr_semantic ctx value in
		(match s_main with SDotExpr (sexpr, x, sem) ->
			let int_type = BaseTypeEntry(look_up_type "Int" ctx.typetab) and 
				double_type = BaseTypeEntry(look_up_type "Double" ctx.typetab) and 
				clip_type = BaseTypeEntry(look_up_type "Clip" ctx.typetab) in
			if sem.type_def = clip_type then (
				if compare_type (extract_semantic s_value).type_def double_type then (
					if compare_type (extract_semantic s_time).type_def int_type then SFrameSetAttribute (sexpr, x, s_time, s_value)
					else if compare_type (extract_semantic s_time).type_def double_type then STimeSetAttribute (sexpr, x, s_time, s_value)
					else raise (SemanticError ("Attribute assignment index" ^ (string_of_expr time) ^ " has to be of Int of Double type"))
				) else raise (SemanticError ((string_of_expr value) ^ ": rvalue for attribute assignment has to be of Double type"))
			) else raise (SemanticError ("Attribute assignment has to be performed to a Clip, but " ^ (string_of_expr main) ^ "'s type is mismatched"))
			| _ -> raise (ProcessingError ("SetAttribute analysis error when processing " ^ (string_of_s_expr s_main))))
	| IfStmt cl -> 
		let bool_type = BaseTypeEntry(look_up_type "Bool" ctx.typetab) in
		let ctx2 = push_var_env ctx in
		SIfStmt (List.map (* go through each cond_exec *)
		(fun t -> match t with CondExec (x,y) -> let s_stmt_list = List.map (build_stmt_semantic ctx2) y in
			match x with 
			| None -> SCondExec(None,  s_stmt_list) (* the final else has no predicate *)
			| Some cond -> 
				let expr_sem = build_expr_semantic ctx2 cond in (* make sure the predicate has type Bool *)
				if compare_type (extract_semantic expr_sem).type_def bool_type then SCondExec(Some(expr_sem), s_stmt_list)
				else raise (SemanticError ("Condition expression" ^ (string_of_expr cond) 
					^ " in the if statement should be of Bool type")) )
		cl)

	| WhileStmt (pred, stmts) -> 
		let bool_type = BaseTypeEntry(look_up_type "Bool" ctx.typetab) in
		let ctx2 = push_var_env ctx in
		let s_pred = build_expr_semantic ctx2 pred in
		if compare_type (extract_semantic s_pred).type_def bool_type then ()
		else raise (SemanticError ("Condition expression" ^ (string_of_expr pred) 
			^ " in the while statement should be of Bool type"));
		SWhileStmt (s_pred, List.map (build_stmt_semantic ctx2) stmts)
			
	| Continue -> SContinue
	| Break -> SBreak
	| Return exp -> (match exp with 
		| None -> SReturn (None)
		| Some x -> SReturn (Some(build_expr_semantic ctx x)))
	
	| ForIn (varname, expr, stmts) -> 
		let s_expr = build_expr_semantic ctx expr in
		let arr_ele_type = match (extract_semantic s_expr).type_def with
							| ArrayTypeEntry t -> t
							| _ -> raise (SemanticError ((string_of_expr expr)
								^" in the for loop has to be of Array type")) in
		let nested_env = push_var_env ctx in
		let svar = new_var nested_env varname arr_ele_type in
		let s_stmt_list = List.map (build_stmt_semantic nested_env) stmts in
		SForIn(varname, (extract_semantic svar), s_expr, s_stmt_list)
	
	| ForRange (varname, st_expr, ed_expr, stmts, dir) ->
		let int_type = BaseTypeEntry(look_up_type "Int" ctx.typetab) in
		let s_st_expr = build_expr_semantic ctx st_expr and s_ed_expr = build_expr_semantic ctx ed_expr in
		let nested_env = push_var_env ctx in
		let svar = new_var nested_env varname 
			(if compare_type (extract_semantic s_st_expr).type_def int_type
			then let ed_type=(extract_semantic s_ed_expr).type_def in
			(if compare_type ed_type int_type then ed_type 
			else raise (SemanticError ((string_of_expr ed_expr) ^ " is expected to be of type Int")))
			else raise (SemanticError ((string_of_expr st_expr) ^ " is expected to be of type Int"))) in
		let s_stmt_list = List.map (build_stmt_semantic nested_env) stmts in
		SForRange(varname, (extract_semantic svar), s_st_expr, s_ed_expr, s_stmt_list, dir)
																		

let build_func_semantic ctx = function
	FuncDecl (funcName, argList, retype, stmtList) ->
		let ctx2 = push_var_env ctx in (* create a new variable env on top of the old *)
		let sarglist = List.map (*  *)
				(fun x -> match x with VarDecl (name, typename) ->
					let svar = new_var ctx2 name (look_up_type2 typename ctx.typetab) in
					SVarDecl (name, extract_semantic svar)) argList in
		let s_stmtlist = List.map (build_stmt_semantic ctx2) stmtList in
		let expected_ret = look_up_type2 retype ctx.typetab in
		let check_ret_type = function 
			| SReturn expr_option -> 
				let actual_ret_type = (match expr_option with 
					| Some ep -> (extract_semantic ep).type_def | None -> BaseTypeEntry(look_up_type "VOID" ctx.typetab)) in
				if compare_type actual_ret_type expected_ret then () 
				else raise (SemanticError ("Return expressions must have type " ^ (string_of_type expected_ret)
					^ " as defined in function " ^ funcName))
			| _ -> () in
		List.iter (fun x -> match x with 
			| SReturn r -> check_ret_type (SReturn r)
			| SIfStmt ceList -> List.iter check_ret_type
							(List.flatten (List.map (fun ce -> match ce with SCondExec (_, stList) -> stList) ceList))
			| SForIn (_, _, _, stList) -> List.iter check_ret_type stList
			| SForRange (_, _, _, _, stList, _) -> List.iter check_ret_type stList
			| _ -> ()
		) s_stmtlist;	
		SFuncDecl (funcName, sarglist, s_stmtlist, {actions=[]; type_def=expected_ret})

let rec build_type_mem_semantic ctx = function
	| MemFuncDecl memfunc -> SMemFuncDecl (build_func_semantic ctx memfunc) 
	| MemTypeDecl memtype -> SMemTypeDecl (build_type_semantic ctx memtype)
	| MemVarDecl  memvar  -> 
		match memvar with 
		| VarDecl (varname, vartype) -> SMemVarDecl (SVarDecl (varname, 
				{actions=[]; type_def=look_up_type2 vartype ctx.typetab}))
			
and build_type_semantic ctx = function
	| TypeDecl (typename, memlist) -> STypeDecl (typename, List.map (build_type_mem_semantic ctx) memlist)
		
let build_program_semantic ctx = function
	| GlobalStmt stmt -> SGlobalStmt (build_stmt_semantic ctx stmt)
	| GlobalType type_decl -> SGlobalType (build_type_semantic ctx type_decl)
	| GlobalFunc func_decl -> SGlobalFunc (build_func_semantic ctx func_decl)


let build_semantic context program = List.map (build_program_semantic context) program
		


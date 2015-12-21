open Ast
open Sast

let is_primitive_type t_def = match t_def.t_name with
	| "Int" | "Double" | "String" | "Bool" | "Void" -> true
	| _ -> false

let rec generate_type_modifier = function
	| BaseTypeEntry b -> (match b.t_name with 
		| "Int" -> "int" | "Double" -> "double" | "Bool" -> "bool" | "String" -> "string" | "Void" -> "void"
		| _ -> "tr1::shared_ptr<" ^ b.t_actual ^ ">")
	| ArrayTypeEntry a -> "tr1::shared_ptr<vector<" ^ (generate_type_modifier a) ^ ">>"

let extract_array_ele_type t = match t with ArrayTypeEntry a -> a | _ -> raise (GenerationError "Expect an array type here")

let rec generate_expr = function
 	| SLiteral (x, s) -> x
	| SArrayLiteral (x, s)-> "create_array<" ^ (generate_type_modifier (extract_array_ele_type s.type_def)) ^ 
		">({" ^ String.concat ", " (List.map generate_expr x) ^ "})"
	| SVar (x, s) -> x
	| SArrayIndex (x, y, s) -> "*" ^ generate_expr x ^ "[" ^ generate_expr y ^ "]"
	| SDotExpr (x, y, s) -> generate_expr x ^ "." ^ y 
	| SBinop (x, op, y, s) -> "(" ^ (generate_expr x) ^ " " ^ (string_of_op op) ^ " " ^ (generate_expr y) ^ ")"
	| SCall (obj, func_type, el, s) -> func_type.t_actual ^ "::eval(" ^
		(match obj with | None -> "DUMMY_SELF" | Some (expr) -> (generate_expr expr)) ^
		(List.fold_left (fun content x -> content ^ ", " ^ (generate_expr x)) "" el) ^ ")"
	| SBuildArray (ele_type, _, _) -> "create_array<" ^ (generate_type_modifier ele_type) ^ ">({})"
	| SArrayRange (smain, sst, sed, sem) -> "slice_array<" ^ (generate_type_modifier (extract_array_ele_type sem.type_def)) 
		^ ">(" ^ (generate_expr smain) ^ ", " ^ (generate_expr sst) ^ ", " ^ (generate_expr sed) ^ ")"
	| SClipTimeCascade (scl1, scl2, stm, _) -> "layerClip(" ^ (generate_expr scl1) ^ ", " ^ (generate_expr scl2) ^ ", " 
		^ (generate_expr stm) ^ ")"
	| SClipFrameCascade (scl1, scl2, stm, _) -> "layerClip(" ^ (generate_expr scl1) ^ ", " ^ (generate_expr scl2) ^ ", " 
			^ (generate_expr stm) ^ ")"
	| SClipConcat (scl1, scl2, _) -> "addClip(" ^ (generate_expr scl1) ^ ", " ^ (generate_expr scl2) ^ ")"
	| SClipFrameRange (smain, sst, sed, _) -> "clipRange(" ^ (generate_expr smain) ^  ", " ^ (generate_expr sst) ^ ", " ^ (generate_expr sed) ^ ")"
	| SClipTimeRange (smain, sst, sed, _) -> "clipRange(" ^ (generate_expr smain) ^  ", " ^ (generate_expr sst) ^ ", " ^ (generate_expr sed) ^ ")"
	| SClipFrameIndex (smain, sidx, _) -> "clipIndex(" ^ (generate_expr smain) ^  ", " ^ (generate_expr sidx) ^ ")"
	| SClipTimeIndex (smain, sidx, _) -> "clipIndex(" ^ (generate_expr smain) ^  ", " ^ (generate_expr sidx) ^ ")"


let rec generate_cond = function
	| SCondExec (x, l) -> 
		let rec generate_stmt_list = function
		  [] -> ""
	    | hd::tl -> generate_stmt hd ^ generate_stmt_list tl
	    in
		(match x with
		| None -> "else if (true) {\n" ^ generate_stmt_list l ^ "}\n"
		| Some (expr) -> "else if (" ^ generate_expr expr ^ ") {\n" ^ generate_stmt_list l ^ "}\n"
		)

and generate_stmt = function
	| SAssign (x, s) -> 
		(match x with
		| None -> generate_expr s
		| Some (expr) -> 
			let sem = extract_semantic expr in
			(try let _ = List.find (fun x -> match x with NewVar -> true | _ -> false) sem.actions in
				(*(generate_type_modifier sem.type_def) ^ *)"auto " ^ (generate_expr expr)
			with Not_found -> generate_expr expr) 
			^ " = " ^ (generate_expr s)(*let sem = extract_semantic expr in
			(try List.find (fun x -> match x with NewVar -> true | _ -> false) sem.actions; 
				(match sem.type_def.t_name with 
				| "Int" -> "int " ^ generate_expr expr ^ " = " ^ generate_expr s
				| "Double" -> "double" ^ generate_expr expr ^ " = " ^ generate_expr s
				| "String" -> "string" ^ generate_expr expr ^ " = " ^ generate_expr s
				| "Bool" -> "bool" ^ generate_expr expr ^ " = " ^ generate_expr s
				| _ -> "auto " ^ (generate_expr expr) ^ " = make_shared<" ^ 
					(let tm = (generate_type_modifier sem.type_def) in 
						String.sub tm 16 ((String.length tm)-16)) ^ ">()"
				)
			with Not_found -> (generate_expr expr) ^ " = " ^ (generate_expr s))*)
		) 
		^ ";\n"

	| SFrameSetAttribute (sexpr, x, s_time, s_value) -> 
		"setProperty(" ^ (String.concat ", " [(generate_expr sexpr); "\"" ^ x ^ "\""; (generate_expr s_time); (generate_expr s_value)]) ^ ");\n"
	| STimeSetAttribute (sexpr, x, s_time, s_value) -> 
		"setProperty(" ^ (String.concat ", " [(generate_expr sexpr); "\"" ^ x ^ "\""; (generate_expr s_time); (generate_expr s_value)]) ^ ");\n"

	| SIfStmt (l) -> "if(false) {}\n" ^ 
		(let rec generate_cond_list = function
		  [] -> ""
	    | hd::tl -> (generate_cond hd) ^ (generate_cond_list tl)
	    in generate_cond_list l)

	| SForIn (loop_var_name, loop_var_sem, array_expr, stmt_list) -> 
		"for (auto& " ^ loop_var_name ^ " : *(" ^ (generate_expr array_expr) ^ ")) {\n" ^
		(generate_stmt_list  stmt_list) ^ "}\n\n"

	| SForRange (loop_var_name, sem, st, ed, stmt_list,sign) ->
		let startstr = generate_expr st in 
		let endstr = generate_expr ed in 
		"for (auto" ^ loop_var_name ^ "=" ^ startstr ^ ";" ^ loop_var_name ^ 
		(if sign = Inc 
			then "<" ^ endstr ^ ";" ^ loop_var_name ^ "++" 
			else ">" ^ startstr ^ ";" ^ loop_var_name ^ "--") ^ ") {\n" ^
		(generate_stmt_list stmt_list) 
	    ^ "}\n\n"


	| SWhileStmt (x, l) -> "while (" ^ generate_expr x ^ ")" ^ "{\n" ^
		(generate_stmt_list l) ^ "}\n\n"

	| SContinue -> "continue;\n"
	| SBreak -> "break;\n"
	| SReturn (x) -> "return " ^
		(match x with
		| None -> ""
		| Some (expr) -> generate_expr expr) ^ ";\n"

and generate_stmt_list stmt_list = String.concat "\n" (List.map generate_stmt stmt_list)

let generate_var_decl = function
	| SVarDecl (name, s) -> (generate_type_modifier s.type_def) ^ " " ^ name

let generate_init_eval args stmts s = 
	let initialObject = 
		let extract_arg_name = function SVarDecl(arg_name, arg_sem) -> arg_name in
		match args with hd :: tail ->
			let selfName = extract_arg_name hd and typeName = (generate_type_modifier s.type_def) in
			selfName ^ " = " ^ typeName ^ "( new " ^ typeName ^ "(" ^ (String.concat ", " (List.map extract_arg_name tail)) ^ "));\n"
		in
		"static " ^ (generate_type_modifier s.type_def) ^ " eval(" ^
		(String.concat ", " (List.map (
			fun x -> (match x with SVarDecl(arg_name, arg_sem) -> (generate_type_modifier arg_sem.type_def)
			 ^ " " ^ arg_name )) args) ) 
		^ ") {\n" ^ initialObject ^ (generate_stmt_list stmts) ^ "}\n\n"

let generate_eval args stmts s = 
		"static " ^ (generate_type_modifier s.type_def) ^ " eval(" ^
		(String.concat ", " (List.map (
			fun x -> (match x with SVarDecl(arg_name, arg_sem) -> (generate_type_modifier arg_sem.type_def)
			 ^ " " ^ arg_name )) args) ) 
		^ ") {\n" ^ (generate_stmt_list stmts) ^ "}\n\n"



let generate_func parent_name = function
	| SFuncDecl(name, args, stmts, s) -> "struct " ^ parent_name ^ "_" ^ name ^ " {\n" ^
		(generate_eval args stmts s) ^ "\n};\n\n"

let rec generate_type parent_name = function
	| STypeDecl (s, stml) -> 
	let this_name = parent_name ^ "_" ^ s in
  	"struct " ^ parent_name ^ "_" ^ s ^ " {\n" ^ 

	(let generate_member content = function
		| SMemVarDecl v -> content ^ (generate_var_decl v) ^ ";\n" | _ -> raise (GenerationError "Expecting SMemVarDecl here")
	 in List.fold_left generate_member "" (List.filter (fun x -> match x with SMemVarDecl s -> true | _ -> false) stml)) ^

	(let generate_type_eval content = function
		| SMemFuncDecl f -> content ^
			(match f with SFuncDecl(name, args, stmts, s) -> if name <> "eval" then "" else (generate_init_eval args stmts s))
		| _ -> raise (GenerationError "Expecting SMemVarDecl here")
	 in List.fold_left generate_type_eval "" (List.filter (fun x -> match x with SMemFuncDecl s -> true | _ -> false) stml)) ^

	"\n};\n\n" ^

	(let generate_mem_func content = function
		| SMemFuncDecl f -> content ^
			(match f with SFuncDecl(name, args, stmts, s) -> if name="eval" then "" else (generate_func this_name f))
		| _ -> raise (GenerationError "Expecting SMemVarDecl here")
	 in List.fold_left generate_mem_func "" (List.filter (fun x -> match x with SMemFuncDecl s -> true | _ -> false) stml)) ^
	
	(List.fold_left  (fun content x -> content ^ 
		(match x with SMemTypeDecl mtd ->generate_type this_name mtd | _ -> raise (ProcessingError ("Generation error"))) ) "" 
		(List.filter (fun x -> match x with SMemTypeDecl s -> true | _ -> false) stml))

let generate context program = 
	let header = ["\"../src/yolib.h\""] in
	let pre_defined = List.map (fun h ->"#include " ^ h ^ "\n") header in
	String.concat "\n" pre_defined ^  
	"\n/********************INCLUDE END******************/\n" ^
	(List.fold_left (fun content x -> content ^ "struct " ^ x.t_actual ^ ";\n") 
		"" (NameMap.fold (fun k v lst -> v:: lst) context.typetab [])) ^
	"\n/********************DECLARATION END*****************/\n" ^
	(List.fold_left (fun content x -> content ^ (match x with 
		| SGlobalType t -> generate_type "" t 
		| _ -> raise (ProcessingError ("Generation error")))) 
		"" (List.filter (fun x -> match x with SGlobalType s -> true | _ -> false) program)) ^
	"\n/****************TYPE DECLARATION ENDED************/\n" ^
	(List.fold_left (fun content x -> content ^ (match x with 
		| SGlobalFunc t -> generate_func "" t 
		| _ -> raise (ProcessingError ("Generation error")))) 
		"" (List.filter (fun x -> match x with SGlobalFunc s -> true | _ -> false) program)) ^
	"\n/**************FUNCTION DECLARATION ENDED***********/\n" ^
	"int main() {\n" ^ 
	(List.fold_left (fun content x -> content ^ (match x with 
		| SGlobalStmt t -> generate_stmt t 
		| _ -> raise (ProcessingError ("Generation error")))) 
		"" (List.filter (fun x -> match x with SGlobalStmt s -> true | _ -> false) program)) ^
	"return 0;\n}"

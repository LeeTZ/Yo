open Ast
open Sast

let is_primitive_type t_def = match t_def.t_name with
	| "Int" | "Double" | "String" | "Bool" -> true
	| _ -> false

let rec generate_type_modifier = function
	| BaseTypeEntry b -> (match b.t_name with 
		| "Int" -> "int" | "Double" -> "double" | "Bool" -> "bool" | "String" -> "std::string"
		| _ -> "std::shared_ptr<" ^ b.t_actual ^ ">")
	| ArrayTypeEntry a -> "std::shared_ptr<std::vector<" ^ (generate_type_modifier a) ^ ">>"


let rec generate_expr = function
 	| SLiteral (x, s) -> x
	| SArrayLiteral (x, s)-> "ARRAY_TO_VECTOR<" ^ (match s.type_def with ArrayTypeEntry a -> generate_type_modifier a | _ -> raise (GenerationError "Expect an array type here")) ^ 
		">({" ^ String.concat ", " (List.map generate_expr x) ^ "})" (* change to a C++ call *)
	| SVar (x, s) -> x
	| SArrayIndex (x, y, s) -> generate_expr x ^ "[" ^ generate_expr y ^ "]"
	| SDotExpr (x, y, s) -> generate_expr x ^ "." ^ y 
	| SBinop (x, op, y, s) -> generate_expr x ^ " " ^ (string_of_op op) ^ " " ^ generate_expr y
	| SCall (obj, f, el, s) -> String.uppercase(f) ^ "::eval(" ^
		(match obj with | None -> "DUMMY_SELF" | Some (expr) -> (generate_expr expr)) ^
		(List.fold_left (fun content x -> content ^ ", " ^ (generate_expr x)) "" el) ^ ")"


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
		| Some (expr) -> (generate_expr expr) ^ " = " ^ (generate_expr s)(*let sem = extract_semantic expr in
			(try List.find (fun x -> match x with NewVar -> true | _ -> false) sem.actions; 
				(match sem.type_def.t_name with 
				| "Int" -> "int " ^ generate_expr expr ^ " = " ^ generate_expr s
				| "Double" -> "double" ^ generate_expr expr ^ " = " ^ generate_expr s
				| "String" -> "std::string" ^ generate_expr expr ^ " = " ^ generate_expr s
				| "Bool" -> "bool" ^ generate_expr expr ^ " = " ^ generate_expr s
				| _ -> "auto " ^ (generate_expr expr) ^ " = std::make_shared<" ^ 
					(let tm = (generate_type_modifier sem.type_def) in 
						String.sub tm 16 ((String.length tm)-16)) ^ ">()"
				)
			with Not_found -> (generate_expr expr) ^ " = " ^ (generate_expr s))*)
		) 
		^ ";\n"


	| SIfStmt (l) -> "if(false) {}\n" ^ 
		(let rec generate_cond_list = function
		  [] -> ""
	    | hd::tl -> generate_cond hd ^ generate_cond_list tl
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


	| SWhileStmt (x, l) -> "while (" ^ generate_expr x ^ ")" ^ "{" ^
		(generate_stmt_list l) ^ "}"

	| SContinue -> "continue;\n"
	| SBreak -> "break;\n"
	| SReturn (x) -> "return " ^
		(match x with
		| None -> ""
		| Some (expr) -> generate_expr expr) ^ ";\n"

and generate_stmt_list stmt_list = String.concat "\n" (List.map generate_stmt stmt_list)

let generate_var_decl = function
	| SVarDecl (name, s) -> (generate_type_modifier s.type_def) ^ " " ^ name

let generate_eval args stmts s = (generate_type_modifier s.type_def) ^ " eval(" ^
			(String.concat ", " (List.map (
				fun x -> (match x with SVarDecl(arg_name, arg_sem) -> (generate_type_modifier arg_sem.type_def)
				 ^ " " ^ arg_name )) args) ) 
			^ ") {" ^ (generate_stmt_list stmts) ^ "}\n\n"

let generate_func parent_name = function
	| SFuncDecl(name, args, stmts, s) -> "struct " ^ String.uppercase(parent_name ^ "_" ^ name) ^ " {\n" ^
		(generate_eval args stmts s) ^ "\n};\n\n"

let rec generate_type parent_name = function
	| STypeDecl (s, stml) -> 
	let this_name = String.uppercase(parent_name ^ "_" ^ s) in
  	"struct " ^ String.uppercase(parent_name ^ "_" ^ s) ^ " {\n" ^ 

	(let generate_member content = function
		| SMemVarDecl v -> content ^ (generate_var_decl v) ^ ";\n\n" | _ -> raise (GenerationError "Expecting SMemVarDecl here")
	 in List.fold_left generate_member "" (List.filter (fun x -> match x with SMemVarDecl s -> true | _ -> false) stml)) ^

	(let generate_mem_eval content = function
		| SMemFuncDecl f -> content ^
			(match f with SFuncDecl(name, args, stmts, s) -> if name <> "eval" then "" else (generate_eval args stmts s))
		| _ -> raise (GenerationError "Expecting SMemVarDecl here")
	 in List.fold_left generate_mem_eval "" (List.filter (fun x -> match x with SMemFuncDecl s -> true | _ -> false) stml)) ^

	"\n};\n\n" ^

	(let generate_mem_func content = function
		| SMemFuncDecl f -> content ^
			(match f with SFuncDecl(name, args, stmts, s) -> if name="eval" then "" else (generate_func this_name f))
		| _ -> raise (GenerationError "Expecting SMemVarDecl here")
	 in List.fold_left generate_mem_func "" (List.filter (fun x -> match x with SMemFuncDecl s -> true | _ -> false) stml)) ^
	
	(List.fold_left  (fun content x -> content ^ (match x with SMemTypeDecl mtd ->generate_type this_name mtd) ) "" 
		(List.filter (fun x -> match x with SMemTypeDecl s -> true | _ -> false) stml))

let generate program = 
	let header = ["\"yolib.h\""] in
	let pre_defined = List.map (fun h ->"#include " ^ h ^ "\n") header in
	String.concat "\n" pre_defined ^  
	(List.fold_left (fun content x -> content ^ (match x with SGlobalType t -> generate_type "" t)) 
		"" (List.filter (fun x -> match x with SGlobalType s -> true | _ -> false) program)) ^
	"\n/****************TYPE DECLARATION ENDED***********/\n" ^
	(List.fold_left (fun content x -> content ^ (match x with SGlobalFunc t -> generate_func "" t)) 
		"" (List.filter (fun x -> match x with SGlobalFunc s -> true | _ -> false) program)) ^
	"\n/**************FUNCTION DECLARATION ENDED***********/\n" ^
	"int main() {\n" ^ 
	(List.fold_left (fun content x -> content ^ (match x with SGlobalStmt t -> generate_stmt t)) 
		"" (List.filter (fun x -> match x with SGlobalStmt s -> true | _ -> false) program)) ^
	"return 0;\n}"

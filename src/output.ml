open Ast
open Semantic
open Printf

(* the whole output is a string *)
(* need initialization here *)

let rec string_of_expr senv expr = (*senv or sast is the environment with the semantic ast*)
	match expr with
	| Id(s) -> s ^ "_"
	| IntConst(c) -> string_of_int c
	| DoubleConst(d) -> string_of_float d
	| BoolConst(b) -> string_of_bool b
	| StrCon(s) -> "\"" ^ s ^ "\"" 
	| ArrayConst(a) -> (* bu hui xie *)
	| ArrayExpr(s,e) -> string_of_expr senv s ^ "_" ^ "[" ^ string_of_expr senv e ^ "]"

	| Dot_Expr(e,s) -> string_of_expr senv e ^ "." ^ s ^ "_"
	| Uminus(e) -> "-" ^ string_of_expr senv e (* This is not a function *)
	| Not_Unary(e) -> "!" ^ string_of_expr senv e

	| Call(a,b,op) -> (
		match op with
			| Add -> string_of_expr senv a ^ "+" ^ string_of_expr senv b
			| Sub -> string_of_expr senv a ^ "-" ^ string_of_expr senv b
			| Mult -> string_of_expr senv a ^ "*" ^ string_of_expr senv b
			| Div -> string_of_expr senv a ^ "/" ^ string_of_expr senv b
			| Equal -> string_of_expr senv a ^ "==" ^ string_of_expr senv b
			| Neq -> string_of_expr senv a ^ "!=" ^ string_of_expr senv b
			| Less -> string_of_expr senv a ^ "<" ^ string_of_expr senv b
			| Leq -> string_of_expr senv a ^ "<=" ^ string_of_expr senv b
			| Greater -> string_of_expr senv a ^ ">" ^ string_of_expr senv b
			| Geq -> string_of_expr senv a ^ ">=" ^ string_of_expr senv b
			| And -> string_of_expr senv a ^ "&&" ^ string_of_expr senv b
			| Or -> string_of_expr senv a ^ "||" ^ string_of_expr senv b
		)

	| FuncCall(a,b) -> 
		let sf = string_of_expr senv a in
		( match sf with
			| Glbl.log -> 
				let btype = lookup senv b in
				( match btype with
					| Int -> "printf(\"%d\"," ^ string_of_expr senv b ^ ")"
					| Doulbe -> "printf(\"%lf\"," ^ string_of_expr senv b ^ ")"
					| String -> "printf(\"%s\"," ^ string_of_expr senv b ^ ")" (* here is helloWorld!\^0^/*)
				|)
			(* other built-in functions here *)
			| _ -> string_of_expr senv a ^ "(" ^ string_of_expr senv b ^ ")"
		)

	| ParenExpr(a) -> "(" ^ string_of_expr senv a ^ ")"

	| List(a) -> (* need to specify *)
	

let rec string_of_stmt senv stmt =
	match stmt with
	| Expr(a) -> string_of_expr senv a ^ ";\n"
	| Bre_Stmt(stmts) -> 
		"{\n  " ^ String.concat (List.map (string_of_stmt senv) stmts) ^ "\n}" (* what if the stmts is empty? *)
	| Assign(a,b) ->
	| IfStmt ->
	| WhileStmt(e,stmt) -> "while " ^ "(" ^ string_of_expr senv e ^ ")" ^ string_of_stmt senv stmt
	| ForIn ->
	| ForEq ->
	| CONTINUE -> "continue;"
	| BREAK -> "break;"
	| Return(e) -> "return " ^ string_of_expr env e ^ ";"


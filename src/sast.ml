open Ast

type action = 
	| NewVar

let string_of_action = function
	| NewVar -> "new"

type sem = {
	mutable actions: action list;
	type_def: type_entry
}


let string_of_sem s = "$" ^ "type: " ^ (string_of_type_def s.type_def) ^ " " 
											^ (String.concat " | " (List.map string_of_action s.actions)) ^ "$"

type s_expr =                                 (* Expressions*)
  | SLiteral of string * sem			(* int, double, bool, string *)
	| SArrayLiteral of s_expr list * sem
  | SArrayExpr of s_expr * s_expr * sem               (* A[B[3]]*)
	| SVar of string * sem             (* foo *)  
  | SDotExpr of s_expr * string * sem        (* A.B *)
  | SBinop of s_expr * op * s_expr * sem      (* 3+4 *)
  | SCall of s_expr option * string * s_expr list * sem      (* foo(a, b) *)
  | SNoExpr 
  
let rec string_of_s_expr = function
	| SLiteral (str, s) -> str ^ (string_of_sem s)
	| SArrayLiteral (selist, s) -> "[" ^ (List.map string_of_s_expr selist) ^ "]" ^ (string_of_sem s)
	| SArrayExpr (sout, sin, s) -> (string_of_s_expr sout) ^ "[" ^ (string_of_s_expr sin) ^ "]" ^ (string_of_sem s)
	| SVar (id, s) -> id ^ (string_of_sem s)
	| SDotExpr (sexpr, id, s) -> (string_of_s_expr sexpr) ^ "." ^ 
	
let extract_semantic = function
	| SLiteral (_, s) -> s
	| SArrayLiteral (_, s) -> s
  | SArrayExpr (_, _, s) -> s            
	| SVar (_, s) -> s            
  | SDotExpr (_, _, s) -> s       
  | SBinop (_, _, _, s) ->s      
  | SCall (_, _, _, s) ->s

type s_stmt =
  | SAssign of s_expr option * s_expr
  | SIfStmt of s_cond_exec list
  | SForIn of string * sem * s_expr * s_stmt list
  | SForEq of string * sem * s_expr * s_expr * s_stmt list
  | SWhileStmt of s_expr * s_stmt list
  | SContinue 
  | SBreak 
  | SReturn of s_expr option

and s_cond_exec = 
   SCondExec of s_expr option * s_stmt list

type s_var_decl = 
	| SVarDecl of string * sem

and s_func_decl = 
  | SFuncDecl of string * s_var_decl list * s_stmt list * sem

and s_type_decl = 
  | STypeDecl of string * s_type_mem_decl list

and s_type_mem_decl = 
  | SMemVarDecl of s_var_decl
  | SMemFuncDecl of s_func_decl
  | SMemTypeDecl of s_type_decl

and global_ele_decl = 
  | SGlobalStmt of s_stmt
  | SGlobalFunc of s_func_decl
  | SGlobalType of s_type_decl
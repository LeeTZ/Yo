open Ast
open Common

type action = 
	| NewVar

type sem = {
	mutable actions: action list;
	type_def: type_entry
}

type s_expr =                                 (* Expressions*)
  | SLiteral of string * sem			(* int, double, bool, string *)
	| SArrayLiteral of s_expr list * sem
  | SArrayExpr of s_expr * s_expr * sem               (* A[B[3]]*)
	| SVar of string * sem             (* foo *)  
  | SDotExpr of s_expr * string * sem        (* A.B *)
  | SBinop of s_expr * op * s_expr * sem      (* 3+4 *)
  | SCall of expr option * string * s_expr list * sem      (* foo(a, b) *)

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
  | SForIn of s_expr * s_stmt list
  | ForEq of s_expr * s_expr * s_stmt list
  | SWhileStmt of s_expr * s_stmt list
  | SContinue 
  | SBreak 
  | SReturn of s_expr

type s_cond_exec = 
   SCondExec of s_expr option * s_stmt

type s_val_decl = 
	| SValueDecl of string * string

type s_type_mem_decl = 
	| SValueDecl of string * string
	| SFuncDecl of string * s_val_decl list * s_stmt list
	|	STypeDecl of string * s_type_mem_decl list
 
type s_program = 
	| SProgram of s_type_mem_decl list
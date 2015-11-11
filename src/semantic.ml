open Ast

module NameMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y
end)



exception ReturnException of int * int NameMap.t

exception VariableNotDefined of string
exception TypeNotDefined of string

module StringMap = Map.Make(String)

(* t is the typename; m is a map of member => typename *)
type class_type =  { t: string; m: class_type StringMap.t }

(* env is a stack of variable lookup tables, from global to local *)
(* venv: string => string; tenv: string => class_type *)
let compile venv tenv program = 
	let rec lookup id = function
		| [] -> raise (VariableNotDefined id)
		| e :: tail -> (try StringMap.find id e with Not_found -> lookup id tail)
		in
	let rec resolve = function
		| Id x -> (try StringMap.find (lookup x venv) tenv 
			with Not_found -> raise (TypeNotDefined (lookup x venv)))
		| DotExpr (expr, x) -> (try StringMap.find x (resolve expr).m 
			with Not_found -> raise (VariableNotDefined (	(string_of_expr expr) ^ x)))
		in
	resolve programs
	

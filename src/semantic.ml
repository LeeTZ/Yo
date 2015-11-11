open Ast

exception VariableNotDefined of string
exception TypeNotDefined of string

module NameMap = Map.Make(String)

type type_entry =  { 
	name: string; (* type name used in yo *)
	actual: string; (* actual name used in target language *)
	members: type_entry NameMap.t (* map of member => type_entry *)
	}
	
type var_entry = {
	name: string; (* type name used in yo *)
	actual: string; (* actual name used in target language *)
	type_def: type_entry (* type definition *)
	}

(* compile environment: variable symbol table * type environment table *)
type compile_context = {
	vsymtab : var_entry NameMap.t list; (* a stack of variable symbol maps of varname => var_entry *)
	typetab: type_entry NameMap.t (* type environment table: a map of typename => type *)
}

let compile context program = 
	(* lookup a variable from local to global in vsymtab. Usage: look_up_var id context.vsymtab *)
	let rec look_up_var id = function 
		| [] -> raise (VariableNotDefined id)
		| hd :: tail -> (try NameMap.find id hd with Not_found -> look_up_var id tail)
		in
	(* create a new variable for the given name and type in the topmost variable map in vsymtab *)	
	let new_var varName typeName ctx =
		let actual_type = 
			(try NameMap.find typeName ctx.typetab 
			with Not_found -> raise (TypeNotDefined typeName)) in
		{ ctx with vsymtab = 
			(NameMap.add varName {name=varName; actual=varName ^ "_"; type_def=actual_type} 
			(List.hd ctx.vsymtab)) :: (List.tl ctx.vsymtab) }
		in
	(* resolve type for a primary expression *)
	let rec resolve_type ctx = function
		| Id x -> (look_up_var x ctx.vsymtab).type_def
		| DotExpr (expr, x) -> (try NameMap.find x (resolve_type ctx expr).members
			with Not_found -> raise (VariableNotDefined (	x ^ " in " (string_of_expr expr))))
		in
	resolve_type context program
	

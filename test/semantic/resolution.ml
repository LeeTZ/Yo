type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =                                 (* Expressions*)
    IntConst of int                         (* 35 *)
  | DoubleConst of float                    (* 21.4 *)
  | BoolConst of bool                       (* True *)
  | StrConst of string                      (* "ocaml" *)
  | ArrayConst of expr list                 (* [12,23,34,56] *)
  | Id of string                            (* foo *)  
  | Array of expr * expr
  | DotExpr of expr * string
  | Binop of expr * op * expr
  | Log of string
  | Noexpr
	
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
			with Not_found -> raise (VariableNotDefined (	"abc" ^ x)))
		in
	resolve_type context program


let 	int_type = {name="Int"; actual="Integer"; members=NameMap.empty } in
let 	clip_mem = NameMap.empty in
let 	clip_mem = NameMap.add "b" int_type clip_mem in
let	 	clip_type = {name="Clip"; actual="Clip"; members=clip_mem } 
	and venv1 = NameMap.empty 
	and venv2 = NameMap.empty
	and tenv = NameMap.empty in 
let 	venv1 = NameMap.add "a" {name="a"; actual="a"; type_def=clip_type} venv1 
	and venv2 = NameMap.add "b" {name="a"; actual="a"; type_def=int_type} venv2
	and tenv = NameMap.add "Int" int_type tenv in
let tenv = NameMap.add "Clip" clip_type tenv in
compile {vsymtab=[venv2; venv1]; typetab=tenv} (DotExpr (Id "a", "b"));;

(* Expected: type_entry = {name = "Int"; actual = "Integer"; members = <abstr>} *)
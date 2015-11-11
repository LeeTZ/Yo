exception VariableNotDefined of string
exception TypeNotDefined of string

module StringMap = Map.Make(String)

(* t is the typename; m is a map of member => typename *)
type class_type =  { t: string; m: class_type StringMap.t }

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =
    IntConst of int
  | DoubleConst of float
  | BoolConst of bool
  | StringConstId of string
  | ArrayConst of expr list
  | Id of string
  | Array of expr * expr
  | DotExpr of expr * string
  | Binop of expr * op * expr
  | Call of string * expr list
  | Noexpr


let compile venv tenv program = 
	let rec lookup id = function
		| [] -> raise (VariableNotDefined id)
		| e :: tail -> (try StringMap.find id e with Not_found -> lookup id tail)
		in
	let rec resolve = function
		| Id x -> (try StringMap.find (lookup x venv) tenv 
			with Not_found -> raise (TypeNotDefined (lookup x venv)))
		| DotExpr (expr, x) -> (try StringMap.find x (resolve expr).m 
			with Not_found -> raise (VariableNotDefined "abc"))
		in
	resolve program;;


let atype = {t="Int"; m=StringMap.empty } and venv1 = StringMap.empty and tenv = StringMap.empty in 
let venv1 = StringMap.add "a" "Int" venv1 and tenv = StringMap.add "Int" atype tenv in compile [venv1] tenv (Id "a");;


let 	int_type = {t="Int"; m=StringMap.empty } 
	and clip_type = {t="Clip"; m=StringMap.empty } 
	and venv1 = StringMap.empty 
	and venv2 = StringMap.empty
	and tenv = StringMap.empty in 
let 	venv1 = StringMap.add "a" "Clip" venv1 
	and venv2 = StringMap.add "b" "Int" venv2
	and tenv = StringMap.add "Int" int_type tenv in
	let tenv = StringMap.add "Clip" clip_type tenv in
clip_type.m <- StringMap.add "b" int_type clip_type.m ; compile [venv2; venv1] tenv (DotExpr (Id "a", "b"));;
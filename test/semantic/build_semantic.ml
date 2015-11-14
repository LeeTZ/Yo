open Ast
open Common


let 	int_type = {name="Int"; actual="Integer"; members=NameMap.empty; evals=[]} in
let 	clip_mem = NameMap.empty in
let 	clip_mem = NameMap.add "b" int_type clip_mem in
let	 	clip_type = {name="Clip"; actual="Clip"; members=clip_mem; evals=[] } 
	and venv1 = NameMap.empty 
	and venv2 = NameMap.empty
	and tenv = NameMap.empty in 
let 	venv1 = NameMap.add "a" {name="a"; actual="a"; type_def=clip_type} venv1 
	and venv2 = NameMap.add "b" {name="b"; actual="b"; type_def=int_type} venv2
	and tenv = NameMap.add "Int" int_type tenv in
let tenv = NameMap.add "Clip" clip_type tenv in
build_semantic {vsymtab=[venv2; venv1]; typetab=tenv} (Assign (Some(Var "h" ), DotExpr (Var "a", "b")))

(* Expected: type_entry = {name = "Int"; actual = "Integer"; members = <abstr>} *)
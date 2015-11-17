open Ast

let generate_built_in_types = 
	let tt = List.fold_left 
		(fun m t -> NameMap.add t 
			{name=t; actual=(String.lowercase t); members=NameMap.empty; evals=[]} m)
		 (NameMap.empty) ["String";"Bool";"Int";"Double";"Void"] in
	let log_f_args = [{name="str"; actual="str"; type_def=NameMap.find "String" tt}] in
	let log_f = {name="LOG"; actual="std::cout<<"; evals=[{args=log_f_args;ret=None}]; members=NameMap.empty} in 
	let tt = NameMap.add "LOG" log_f tt
	in {vsymtab=[NameMap.empty]; typetab=tt}

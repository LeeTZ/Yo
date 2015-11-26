open Ast

let generate_built_in_types = 
	let tt = List.fold_left 
		(fun m t -> NameMap.add t
			{name=t; actual=t; members=NameMap.empty; evals=[]} m)
		 (NameMap.empty) ["String";"Bool";"Int";"Double";"Void"] in
	let log_f_args_str = [{name="str"; actual="str"; type_def=NameMap.find "String" tt}] 
	and log_f_args_int = [{name="integer"; actual="integer"; type_def=NameMap.find "Int" tt}]
	and log_f_args_double = [{name="float_number"; actual="float_number"; type_def=NameMap.find "Double" tt}] in
	let log_f = {name="LOG"; actual="LOG"; evals=[
							{args=log_f_args_str;ret=None}; 
							{args=log_f_args_int;ret=None}; 
							{args=log_f_args_double;ret=None}]; 
				members=NameMap.empty} in 
	let tt = NameMap.add "LOG" log_f tt
	in {vsymtab=[NameMap.empty]; typetab=tt}

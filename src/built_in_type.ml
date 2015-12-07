open Ast

let generate_built_in_types = 
	let tt = List.fold_left 
		(fun m t -> NameMap.add t
			{t_name=t; t_actual=t; members=NameMap.empty; evals=[]} m)
		 (NameMap.empty) ["String";"Bool";"Int";"Double";"Void";"$"] in
	let log_f_args_str = [{v_name="str"; v_actual="str"; v_type=BaseTypeEntry (NameMap.find "String" tt)}] 
	and log_f_args_int = [{v_name="integer"; v_actual="integer"; v_type=BaseTypeEntry (NameMap.find "Int" tt)}]
	and log_f_args_double = [{v_name="float_number"; v_actual="float_number"; v_type=BaseTypeEntry(NameMap.find "Double" tt)}] in
	let log_f = {t_name="LOG"; t_actual="LOG"; evals=[
							{args=log_f_args_str;ret=BaseTypeEntry(NameMap.find "Void" tt)}; 
							{args=log_f_args_int;ret=BaseTypeEntry(NameMap.find "Void" tt)}; 
							{args=log_f_args_double;ret=BaseTypeEntry(NameMap.find "Void" tt)}];
				members=NameMap.empty} in 
	let tt = NameMap.add "LOG" log_f tt
	in {vsymtab=[NameMap.empty]; typetab=tt}

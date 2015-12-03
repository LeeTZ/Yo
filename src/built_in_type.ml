open Ast

let generate_built_in_types = 
	let tt = List.fold_left 
		(fun m t -> NameMap.add t
			{t_name=t; t_actual=t; is_array=false; members=NameMap.empty; evals=[]} m)
		 (NameMap.empty) ["String";"Bool";"Int";"Double";"Void";"$"] in
	let log_f_args_str = [{v_name="str"; v_actual="str"; type_def=NameMap.find "String" tt}] 
	and log_f_args_int = [{v_name="integer"; v_actual="integer"; type_def=NameMap.find "Int" tt}]
	and log_f_args_double = [{v_name="float_number"; v_actual="float_number"; type_def=NameMap.find "Double" tt}] in
	let log_f = {t_name="LOG"; t_actual="LOG"; evals=[
							{args=log_f_args_str;ret=NameMap.find "Void" tt}; 
							{args=log_f_args_int;ret=NameMap.find "Void" tt}; 
							{args=log_f_args_double;ret=NameMap.find "Void" tt}]; 
				is_array=false;
				members=NameMap.empty} in 
	let tt = NameMap.add "LOG" log_f tt
	in {vsymtab=[NameMap.empty]; typetab=tt}

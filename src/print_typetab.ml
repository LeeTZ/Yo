open Ast

let rec print_typeentry curtype = function
	| BaseTypeEntry(be) -> (curtype ^ be.t_name)
	| ArrayTypeEntry(ae) -> (print_typeentry (curtype^"array of ") ae)

and print_varentry k = 
	print_string("v_name: " ^ k.v_name ^ ", v_actual: " ^ k.v_actual);
	print_string (print_typeentry "" k.v_type);
	print_string "\n";

and print_memberentry k v = 
	print_string ("Key: "^k^"\n");

and print_basetype k (bt:base_type) =
	print_string ("Typetab KEY: " ^ k ^ "\n");
	print_string ("t_name: "^bt.t_name^", ");
	print_string ("t_actual: "^bt.t_actual^", "^"\n");
	print_string "Eval:\n";
	List.iter (fun x -> print_evalentry x) bt.evals;
	print_string ("Member: "^"\n");
	NameMap.iter print_memberentry bt.members;
	print_string ("\n");

and print_evalentry ee = 
	print_string "args: ";
	List.iter (fun k -> print_varentry k;()) ee.args;
	print_string "ret: ";
	print_string (print_typeentry "" ee.ret);
	print_string "\n";

and printtypetab t =
   NameMap.iter (fun k v-> print_basetype k v) t;
    
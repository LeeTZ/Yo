open Ast

let rec exists_types id = function
    [] -> false
    | sym_entry :: rest -> if sym_entry.name = id then
                            if sym_entry.v_type <> [Unknown] then true
                            else false
                           else exists_types id rest

let rec types_to_s_type = function
      Int -> Ast.Int
    | Bool -> Ast.Bool
    | Double -> Ast.Float
    | String -> Ast.String
    | Array(a) -> Ast.Array(types_to_s_type a)

let rec walk_decl typeList = function
      Ast.TypeDef(id, type_element) -> 
                let entry = {name=id; actual=id; members=(List.Map types_to_s_type type_element) } in 
                if (exists_types id typeList)
                    then raise (Multiple_type_error id)
                else 
                	entry::typeList
    | Ast.FuncDecl(id, arglist, stmtlist) ->
    		let entry = {name=id; actual=id; members=NameMap.empty} in
            if (exists_types if typeName)
                then raise (Multiple_type_error id)
            else
                entry::typeList
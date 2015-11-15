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


let rec args_map args_hash id = function
    Ast.VarDecl(var, typename) ->
            if (exists_types typenametab) = false
                then raise (Type_not_defined typename)
            else
                let args_hash = NameMap.add var typename args_hash in
                    args_hash

let rec type_nested typetab id = function
    Ast.TypeDecl(tid, type_element) -> 
            if (exists_types id typetab)
                then raise (Multiple_type_error id)
            else 
                let entry = {name=id ^ "." ^ tid; actual=id ^ "." ^ tid; evals=None; evals=None; members=List.Map (fun id -> type_nested typetab id) type_element } in 
                let typetab = NameMap.add entry.name entry typetab
                    in typetab

    | Ast.FuncDecl(id, arglist, stmtlist) ->
            if (exists_types in typetab)
                then raise (Multiple_type_error id)
            else
                let args_list = 
                    entry = {name=String.uppercase id; actual=String.uppercase id; evals=None; members=NameMap.empty} in
                let typetab = NameMap.add entry.name entry typetab
                    in typetab

(* First pass to scan all type names *)
let rec walk_decl_1 typetab = function
      Ast.TypeDecl(id, type_element) -> 
            if (exists_types id typetab)
                then raise (Multiple_type_error id)
            else 
                let entry = {name=id; actual=id; evals=None; members=walk_decl_1 typetab type_element} in 
                let typetab = NameMap.add entry.name entry typetab

    | Ast.FuncDecl(id, arglist, stmtlist) ->
            if (exists_types in typetab)
                then raise (Multiple_type_error id)
            else
                let entry = {name=String.uppercase id; actual=String.uppercase id; evals=List.empty; members=NameMap.empty} in
                let typetab = NameMap.add entry.name entry typetab

let rec args_add arglist typetab = function
      Ast.VarDecl(name, typename) ->
        let NameMap.add typetab.evals name typename in 
            typetab

(* Second pass to scan all members and evals *)
let rec walk_decl_2 memberList typetab = function
    | Ast.FuncDecl(id, arglist, stmtlist) ->  
        if (exists_type arglist) = false
            then raise (Type_not_defined arglist)
        else
            let typetab = args_add arglist typetab
                in typetab 

    | Ast.VarDecl(name, typename) ->  
        if (exists_types typename) = false  
            then raise (Type_not_defined typename)





open Ast

let walk_dec program context = 
    let print_kv k v =
        print_string(k ^ "\n")

    in

    let printtypetab t = 
        NameMap.iter print_kv t
    in

    let generate_scope parent_scope id = (if parent_scope="" then "" else (String.uppercase parent_scope) ^ "::") ^ (String.uppercase id) 
    in

    let generate_var_type id tail = if tail="" then (String.uppercase id) else ((String.uppercase id)^"::")^ (String.uppercase tail) 

    in

    let exists_types_id typetab id isarray = (try let typeEntry = NameMap.find id typetab in
                (if isarray=1 then (ArrayTypeEntry(BaseTypeEntry(typeEntry))) else (BaseTypeEntry(typeEntry)) )
            with Not_found -> raise (TypeNotDefined id))
    in

    let rec exists_types typetab tail isarray = function
        | SimpleType(st) -> exists_types_id typetab (generate_var_type st tail) isarray
        | NestedType(nt,id) -> exists_types typetab (generate_var_type id tail) isarray nt
        | ArrayType(at) -> exists_types typetab tail 1 at
    in
    

    let rec type_nested_walk_1 typetab oid = function
        | Ast.TypeDecl(id, type_element) -> 
                let newid = generate_scope oid (String.uppercase id) in
                let typeEntry = {t_name=newid; t_actual=newid; evals=[]; members=NameMap.empty;} in
                let tt = NameMap.add newid typeEntry typetab in  
                    List.fold_left (fun tt e -> mem_nested_1 tt newid e) tt type_element 

    and func_nested_walk_1 typetab oid = function
        | Ast.FuncDecl(id, arglist, retype, stmtlist) ->
                if id<>"eval" then (
                let newid = generate_scope oid (String.uppercase id) in
                let typeEntry = {t_name=newid; t_actual=newid; evals=[]; members=NameMap.empty;} in
                let tt = NameMap.add newid typeEntry typetab in
                tt) else typetab 


    and mem_nested_1 typetab id = function
        | Ast.MemTypeDecl(typedecl) -> 
            type_nested_walk_1 typetab id typedecl
        | Ast.MemFuncDecl(funcdecl) ->
            func_nested_walk_1 typetab id funcdecl 
        | _ -> typetab
    in

    let funcwalk_1 typetab parent_scope = function
        | Ast.FuncDecl(id, arglist, retype, stmtlist) ->
                let newid = generate_scope parent_scope (String.uppercase id) in
                let entry = {t_name=newid; t_actual=newid; evals=[]; members=NameMap.empty} in
                let tt = NameMap.add newid entry typetab in 
                tt
        
    and typewalk_1 typetab parent_scope= function
         | Ast.TypeDecl(id, type_element) -> 
                let newid = generate_scope parent_scope (String.uppercase id) in 
                let entry = {t_name=newid; t_actual=newid; members=NameMap.empty; evals=[]} in
                let tt = NameMap.add newid entry typetab in
                    List.fold_left (fun tt e -> mem_nested_1 tt id e) tt type_element 
        in

    let walk_decl_1 typetab = function
          | Ast.GlobalType(type_decl) -> let tt = typewalk_1 typetab "" type_decl in tt
          | Ast.GlobalFunc(func_decl) -> let tt = funcwalk_1 typetab "" func_decl in tt
          | _ -> typetab
     in


    let varwalk_2 typetab parent_type = function
        | Ast.VarDecl(name, typename) ->
            let memVarEntry = exists_types typetab "" 0 typename in
                NameMap.add name memVarEntry parent_type.members 
    in

    let funcwalk_2 typetab parent_scope = function
        | Ast.FuncDecl(id, arglist, retype, stmtlist) ->
            let scope = if id <> "eval" then (generate_scope parent_scope (String.uppercase id) ) else parent_scope in
            let f_type = NameMap.find scope typetab in
            f_type.evals <-
            {args=(List.map (fun x -> match x with 
                VarDecl(n,t) -> {v_name=n; v_actual=n^"_"; v_type=(exists_types typetab "" 0 t)})
                arglist); ret=(exists_types typetab "" 0 retype)} :: f_type.evals

    in

    let rec typewalk_2 typetab parent_scope = function
        | Ast.TypeDecl(id, ele_list) -> 
            let newid = generate_scope parent_scope (String.uppercase id) in
            List.iter (fun e -> match e with
                | Ast.MemFuncDecl(mf) -> funcwalk_2 typetab newid mf; ()
                | Ast.MemVarDecl(mv) -> varwalk_2 typetab (NameMap.find newid typetab) mv; ()
                | Ast.MemTypeDecl(mt) -> typewalk_2 typetab newid mt
            ) ele_list
    in

    let rec walk_decl_2 typetab = function
        | Ast.GlobalType(type_decl) -> typewalk_2 typetab "" type_decl; typetab
        | Ast.GlobalFunc(func_decl) -> funcwalk_2 typetab "" func_decl; typetab
        | _ -> typetab
    in

    let first_pass tt program = 
        let listpass = List.fold_left (fun tt e -> walk_decl_1 tt e) tt program 
            in listpass
    and second_pass tt program = 
        let listpass = List.fold_left (fun tt e -> walk_decl_2 tt e) tt program
            in listpass
in let t = context.typetab in let t = first_pass t program in let t = second_pass t program in
{vsymtab=[]; typetab=t} 

(*
let context = walk_dec
[GlobalType(TypeDecl("typetest",[MemVarDecl(VarDecl("a","Int"))]))]  *)

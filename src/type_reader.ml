open Ast

let walk_dec program context =

    (*let print_kv k v =
        print_string("key is " ^ k ^ ", t_name: " ^ v.t_name ^ ", t_actual:" ^ v.t_actual)

    let printtypetab t = 
        NameMap.iter print_kv t
    in*)

    let generate_scope parent_scope id = (if parent_scope="" then "" else (String.uppercase parent_scope) ^ "::") ^ (String.uppercase id) 
    in

    let generate_var_type id tail = if tail="" then (String.uppercase id) else ((String.uppercase id)^"::")^ (String.uppercase tail) 
    in

    let rec wrapArray curtype arrcnt =
        if arrcnt = 0 then curtype else (wrapArray (ArrayTypeEntry(curtype)) (arrcnt-1))
    in 

    let duplicate_types typetab id = 
        try 
            let _ = NameMap.find id typetab in
                raise (TypeRedefined id)
        with   
            Not_found -> id
    in

    let exists_types_id typetab basename arrcnt = 
            try 
                let be = (NameMap.find basename typetab) in 
                let curtype = wrapArray (BaseTypeEntry(be)) arrcnt in
                curtype
            with Not_found -> raise (TypeNotDefined basename)
    in

    let rec exists_types typetab tail arrcnt = function
        | SimpleType(st) -> 
                let basename = generate_var_type st tail in
                    exists_types_id typetab basename arrcnt
        | NestedType(nt,id) -> 
                let basename = generate_var_type id tail in
                    exists_types typetab basename arrcnt nt
        | ArrayType(at) -> 
                exists_types typetab tail (arrcnt+1) at
    in
    

    let rec type_nested_walk_1 typetab oid = function
        | Ast.TypeDecl(id, type_element) -> 
                let newid = generate_scope oid (String.uppercase id) in
                let _ = duplicate_types typetab newid in 
                let typeEntry = {t_name=newid; t_actual=newid; evals=[]; members=NameMap.empty;} in
                let tt = NameMap.add newid typeEntry typetab in  
                    List.fold_left (fun tt e -> mem_nested_1 tt newid e) tt type_element 

    and func_nested_walk_1 typetab oid = function
        | Ast.FuncDecl(id, arglist, retype, stmtlist) ->
                if id<>"eval" then (
                let newid = generate_scope oid (String.uppercase id) in
                let _ = duplicate_types typetab newid in 
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
                let _ = duplicate_types typetab newid in 
                let entry = {t_name=newid; t_actual=newid; evals=[]; members=NameMap.empty} in
                let tt = NameMap.add newid entry typetab in 
                tt
        
    and typewalk_1 typetab parent_scope= function
         | Ast.TypeDecl(id, type_element) -> 
                let newid = generate_scope parent_scope (String.uppercase id) in 
                let _ = duplicate_types typetab newid in 
                let entry = {t_name=newid; t_actual=newid; members=NameMap.empty; evals=[]} in
                let tt = NameMap.add newid entry typetab in
                    List.fold_left (fun tt e -> mem_nested_1 tt id e) tt type_element 
        in

    let walk_decl_1 typetab = function
          | Ast.GlobalType(type_decl) -> let tt = typewalk_1 typetab "" type_decl in tt
          | Ast.GlobalFunc(func_decl) -> let tt = funcwalk_1 typetab "" func_decl in tt
          | _ -> typetab
     in


    let varwalk_2 typetab parent = function
        | Ast.VarDecl(name, typename) ->
            
            let memVarEntry = exists_types typetab "" 0 typename in
            let parent_base = NameMap.find parent typetab in
            parent_base.members <- NameMap.add name memVarEntry parent_base.members

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
                | Ast.MemVarDecl(mv) -> varwalk_2 typetab newid mv; ()
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
{vsymtab=[NameMap.empty]; typetab=t} 

(*
let context = walk_dec
[GlobalType(TypeDecl("typetest",[MemVarDecl(VarDecl("a","Int"))]))]  *)

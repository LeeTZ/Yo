open Ast

let walk_dec program context = 
    let print_kv k v =
    print_string(k ^ "\n")

    in

    let printtypetab t = 
        NameMap.iter print_kv t
    in

    let exists_types typetab id = (try NameMap.find id typetab 
            with Not_found -> raise (TypeNotDefined id))
    in

    let generate_scope parent_scope id = (if parent_scope="" then "" else (String.uppercase parent_scope) ^ "::") ^ (String.uppercase id) 
    in

    let rec type_nested_walk_1 typetab oid = function
        | Ast.TypeDecl(id, type_element) -> 
                let newid = generate_scope oid (String.uppercase id) in
                let tt = NameMap.add newid {name=newid; actual=newid; evals=[]; members=MemberMap.empty;} typetab in  
                    List.fold_left (fun tt e -> mem_nested_1 tt newid e) tt type_element 
        | _ -> typetab

    and func_nested_walk_1 typetab oid = function
        | Ast.FuncDecl(id, arglist, stmtlist) ->
                let newid = generate_scope oid (String.uppercase id) in
                let tt = NameMap.add newid {name=newid; actual=newid; evals=[]; members=NameMap.empty;} typetab in
                tt
        | _ -> typetab

    and mem_nested_1 typetab id = function
        | Ast.MemTypeDecl(typedecl) -> 
            type_nested_walk_1 typetab id typedecl
        | Ast.MemFuncDecl(funcdecl) ->
            func_nested_walk_1 typetab id funcdecl 
        | _ -> typetab
    in

    let funcwalk_1 typetab parent_scope = function
        | Ast.FuncDecl(id, arglist, stmtlist) ->
                let newid = generate_scope parent_scope (String.uppercase id) in
                let entry = {name=newid; actual=newid; evals=[]; members=NameMap.empty} in
                let tt = NameMap.add entry.name entry typetab in
                tt
        | _ -> typetab
        
    and typewalk_1 typetab parent_scope= function
         | Ast.TypeDecl(id, type_element) -> 
                let newid = generate_scope parent_scope (String.uppercase id) in 
                let entry = {name=newid; actual=newid; members=NameMap.empty; evals=[]} in
                let tt = NameMap.add newid entry typetab in
                    List.fold_left (fun tt e -> mem_nested_1 tt id e) tt type_element 
                    
        | _ -> typetab
        in

    let walk_decl_1 typetab = function
          | Ast.GlobalType(type_decl) -> let tt = typewalk_1 typetab "" type_decl in tt
          | Ast.GlobalFunc(func_decl) -> let tt = funcwalk_1 typetab "" func_decl in tt
          | _ -> typetab
     in

    let varwalk_2 typetab typeEntry = function
        | Ast.VarDecl(name, typename) ->
            (typeEntry.members <- NameMap.add name (exists_types typetab typename) typeEntry.members); 
    in

    let funcwalk_2 typetab parent_scope = function
        | Ast.FuncDecl(id, arglist, stmtlist) ->
            let scope = generate_scope parent_scope id in
            let f_type = NameMap.find scope typetab in
            f_type.evals <-
            {args=(List.map (fun x -> match x with 
                VarDecl(n,t) -> {name=n; actual=n^"_"; type_def=(exists_types typetab t)})
                arglist); ret=None} :: f_type.evals
    in

    let rec typewalk_2 typetab parent_scope = function
        | Ast.TypeDecl(id, ele_list) -> 
            let newid = generate_scope parent_scope id in
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

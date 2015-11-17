open Ast

let walk_dec program = 
    let exists_types typetab id = (try NameMap.find id typetab 
            with Not_found -> raise (TypeNotDefined id))
    in

    let generate_scope parent_scope id = (if parent_scope="" then "" else parent_scope ^ "::") ^ (String.uppercase id) 
    in

    let rec list_loop_1 typetab id = function
        | [] ->  typetab
        | [hd] -> mem_nested_1 typetab id hd
        | _::tl -> list_loop_1 typetab id tl

    and type_nested_walk_1 typetab oid = function
         | Ast.TypeDecl(id, type_element) -> 
                try
                    exists_types typetab id;
                    typetab
                with
                    Not_found -> 
                        let entry = {name=oid ^ "::" ^ id; actual=oid ^ "::" ^ id; evals=[]; members=MemberMap.empty;}  
                            in NameMap.add entry.name entry typetab;
                        list_loop_1 typetab id type_element;
                        typetab

    and func_nested_walk_1 typetab oid = function
        | Ast.FuncDecl(id, arglist, stmtlist) ->
                try
                    exists_types typetab id;
                    typetab
                with
                    Not_found -> 
                        let entry = {name=oid ^ "::" ^ (String.uppercase id); actual=oid ^ "::" ^ (String.uppercase id); 
                        evals=[]; members=NameMap.empty} in NameMap.add entry.name entry typetab  

    and mem_nested_1 typetab id = function
        | Ast.MemTypeDecl(typedecl) -> 
            type_nested_walk_1 typetab id typedecl
        | Ast.MemFuncDecl(funcdecl) ->
            func_nested_walk_1 typetab id funcdecl 
    in

    let funcwalk_1 typetab = function
        | Ast.FuncDecl(id, arglist, stmtlist) ->
                (try
                    exists_types typetab id;
                    typetab
                with
                    Not_found -> 
                        let entry = {name=String.uppercase id; actual=String.uppercase id; evals=[]; members=NameMap.empty} in
                        NameMap.add entry.name entry typetab )
        
    and typewalk_1 typetab = function
         | Ast.TypeDecl(id, type_element) -> 
                (try
                    exists_types typetab id;
                    typetab
                with
                    Not_found ->
                        let entry = {name=id; actual=id; evals=[]; members=MemberMap.empty;} in 
                            NameMap.add entry.name entry typetab;
                        list_loop_1 typetab id type_element;
                        typetab)
        in

    let walk_decl_1 typetab = function
          | Ast.GlobalType(type_decl) -> typewalk_1 typetab type_decl
          | Ast.GlobalFunc(func_decl) -> funcwalk_1 typetab func_decl
          | _ -> typetab
     in
    (*in

    List.iter (fun e -> walk_decl_1 typetab e) [GlobalType(TypeDecl("typetest",[MemVarDecl(VarDecl("a","Int"))]))]*)


    let varwalk_2 typetab typeEntry = function
        | Ast.VarDecl(name, typename) ->
            (typeEntry.members <- NameMap.add name (exists_types typetab typename) typeEntry.members)
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
            let scope = generate_scope parent_scope id in
            List.iter (fun e -> match e with
                | Ast.MemFuncDecl(mf) -> funcwalk_2 typetab scope mf; ()
                | Ast.MemVarDecl(mv) -> varwalk_2 typetab (NameMap.find scope typetab) mv; ()
                | Ast.MemTypeDecl(mt) -> typewalk_2 typetab scope mt
            ) ele_list
    in


    let rec walk_decl_2 typetab = function
        | Ast.GlobalType(type_decl) -> typewalk_2 typetab "" type_decl; typetab
        | Ast.GlobalFunc(func_decl) -> funcwalk_2 typetab "" func_decl; typetab
        | _ -> typetab
    in

    let first_pass tt program = List.iter (fun e -> walk_decl_1 tt e;()) program; tt
    and second_pass tt program = List.iter (fun e -> walk_decl_2 tt e;()) program; tt
in let t = NameMap.empty in let t = first_pass t program in let t = second_pass t program in
{vsymtab=[]; typetab=t}

(*
let context = walk_dec
[GlobalType(TypeDecl("typetest",[MemVarDecl(VarDecl("a","Int"))]))] *)

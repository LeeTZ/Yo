open Ast
open Common

module NameMap = Map.Make(String)

let typetab = NameMap.empty
let exists_types typetab id =  NameMap.find id typetab
let rec args_add typetab = function
      Ast.VarDecl(name, typename) ->
        try
            NameMap.add typetab.evals name typename
        with
            Not_found -> raise (TypeNotDefined typename)
let rec type_nested typetab id = function
    | Ast.TypeDecl(tid, type_element) -> 
            try
                exists_types typetab tid
            with
                Not_found -> 
                    let entry = {name=id ^ "::" ^ tid; actual=id ^ "::" ^ tid; evals=None; evals=None; members=List.Map (fun id -> type_nested typetab id) type_element } in
                    NameMap.add entry.name entry typetab
    | Ast.FuncDecl(id, arglist, stmtlist) ->
            try
                exists_types typetab id
            with 
                Not_found ->
                    let args_list = 
                        entry = {name=String.uppercase id; actual=String.uppercase id; evals=None; members=NameMap.empty} in
                            NameMap.add entry.name entry typetab
(* First pass to scan all type names *)
let rec walk_decl_1 typetab = function
      Ast.TypeDecl(id, type_element) -> 
            try
                exists_types typetab id
            with
                Not_found ->
                    let entry = {name=id; actual=id; evals=None; members=List.Map (fun id -> type_nested typetab id) type_element} in 
                        NameMap.add entry.name entry typetab 
    | Ast.FuncDecl(id, arglist, stmtlist) ->
            try
                exists_types typetab id
            with
                Not_found -> 
                    let entry = {name=String.uppercase id; actual=String.uppercase id; evals=List.empty; members=NameMap.empty} in
                        NameMap.add entry.name entry typetab                
in walk_decl_1 typetab
(* Second pass to scan all members and evals *)
let rec walk_decl_2 typetab = function
    | Ast.FuncDecl(id, arglist, stmtlist) ->  
        List.map (fun arglist -> walk_decl_2 typetab arglist) arglist
    | Ast.VarDecl(name, typename) ->  
        try 
            exists_types typetab typename
        with
            Not_found -> raise (TypeNotDefined typename)
    | Ast.TypeDef(id, type_element) ->
        List.map (fun type_element -> walk_decl_2 typetab type_element) type_element
in walk_decl_2 typetab



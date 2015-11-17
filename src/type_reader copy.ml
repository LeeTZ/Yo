
open Ast
exception VariableNotDefined of string
exception TypeNotDefined of string
exception SemanticError of string
exception TypeExist of string

module NameMap = Map.Make(String)
module MemberMap = Map.Make(String)

type eval_entry = {
    mutable args: var_entry list;
    mutable ret: type_entry option
    }
and type_entry =  { 
  name: string; (* type name used in yo *)
  actual: string; (* actual name used in target language *)
    mutable evals: eval_entry list; (* a list of eval functions *)
  mutable members: type_entry NameMap.t (* map of member_name => type_entry *)
  }
and var_entry = {
  name: string; (* type name used in yo *)
  actual: string; (* actual name used in target language *)
  type_def: type_entry (* type definition *)
  }

(* compile environment: variable symbol table * type environment table *)
type compile_context = {
  mutable vsymtab: var_entry NameMap.t list; (* a stack of variable symbol maps of varname => var_entry *)
  mutable typetab: type_entry NameMap.t (* type environment table: a map of typename => type *)
}


let program = ([GlobalType(TypeDecl("typetest",[MemVarDecl(VarDecl("a","Int"))]))])

let exists_types typetab id =  NameMap.find id typetab

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

and funcwalk_1 typetab = function
    | Ast.FuncDecl(id, arglist, stmtlist) ->
            try
                exists_types typetab id;
                typetab
            with
                Not_found -> 
                    let entry = {name=String.uppercase id; actual=String.uppercase id; 
                    evals=[]; members=NameMap.empty} in
                        NameMap.add entry.name entry typetab 
    
and typewalk_1 typetab = function
     | Ast.TypeDecl(id, type_element) -> 
            try
                exists_types typetab id;
                typetab
            with
                Not_found ->
                    let entry = {name=id; actual=id; evals=[]; members=MemberMap.empty;} in 
                        NameMap.add entry.name entry typetab;
                    list_loop_1 typetab id type_element;
                    typetab


let rec walk_decl_1 typetab = function
      | Ast.GlobalType(type_decl) -> typewalk_1 typetab type_decl
      | Ast.GlobalFunc(func_decl) -> funcwalk_1 typetab func_decl


and list_loop_var memtab = function
    | [] -> memtab
    | [hd] -> memvarwalk_2 memtab hd
    | _::tl -> list_loop_var memtab tl

and memvarwalk_2 memtab = function 
    | Ast.MemVarDecl(mv) -> varwalk_2 memtab mv

and varwalk_2 memtab = function
    | Ast.VarDecl(name, typename) ->
         NameMap.add name typename memtab

and list_loop_func typetab evaltab = function
    | [] -> evaltab
    | [hd] -> memfuncwalk_2 typetab evaltab hd
    | _::tl -> list_loop_func typetab evaltab tl

and memfuncwalk_2 typetab evaltab = function 
    | Ast.MemFuncDecl(mf) -> funcwalk_2 typetab evaltab mf

and funcwalk_2 typetab evaltab = function
    | Ast.FuncDecl(id, arglist, stmtlist) ->
        evaltab <- (List.map (fun x -> match x with 
            VarDecl(name,typename) -> {name=x.name; actual=x.name^"_"; type_def=(exists_types typetab typename)})
            arglist) :: evaltab

and list_loop_type typetab = function
    | [] -> typetab
    | [hd] -> memtypewalk_2 typetab hd
    | _::tl -> list_loop_type typetab tl

and memtypewalk_2 typetab = function
    | Ast.MemTypeDecl(mt) -> type_walk_2 typetab mt

and type_walk_2 typetab = function
    | Ast.TypeDecl(id, type_element) -> 
        list_loop_type  typetab type_element;
        list_loop_func (NameMap.find id typetab).evals type_element;
        list_loop_var (NameMap.find id typetab).memebers type_element

let rec walk_decl_2 typetab = function
    | Ast.GlobalType(type_decl) -> typewalk_2 typetab type_decl
    | Ast.GlobalFunc(func_decl) -> funcwalk_2 typetab func_decl







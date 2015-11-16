
open Ast
exception VariableNotDefined of string
exception TypeNotDefined of string
exception SemanticError of string
exception TypeExist of string

module NameMap = Map.Make(String)

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


let typetab = NameMap.empty
let program = ([GlobalType(TypeDecl("typetest",[MemVarDecl(VarDecl("a","Int"))]))])

let exists_types typetab id =  NameMap.find id typetab

let rec func_unit_nested_1 typetab id = function
    | Ast.FuncDecl(fid, arglist, stmtlist) ->
            try
                exists_types typetab fid
            with 
                Not_found ->
                    let args_list = 
                        let entry={name=String.uppercase fid; actual=String.uppercase fid;
                        evals=[]; members=NameMap.empty} in
                            NameMap.add entry.name entry typetab
and type_unit_nested_1 typetab id = function
    | Ast.TypeDecl(tid,type_element) -> 
            try
                exists_types typetab tid
            with
                Not_found -> 
                    let entry = {name=id ^ "::" ^ tid; actual=id ^ "::" ^ tid; evals=[]; 
                        members=(List.map (fun te -> (type_nested_1 typetab (id ^ "::" ^ tid) te)) type_element);} in
                    NameMap.add entry.name entry typetab 

and type_nested_1 typetab id = function
    | Ast.MemTypeDecl(typedecl) -> 
            type_unit_nested_1 typetab id typedecl 
    | Ast.MemFuncDecl(funcdecl) ->
            func_unit_nested_1 typetab id funcdecl
    
and typewalk_1 typetab = function
     | Ast.TypeDecl(id, type_element) -> 
            try
                exists_types typetab id
            with
                Not_found ->
                    let entry = {name=id; actual=id; evals=None; members=(List.map (fun te -> type_nested_1 typetab id te) type_element);} in 
                        NameMap.add entry.name entry typetab 

and funcwalk_1 typetab = function
    | Ast.FuncDecl(id, arglist, stmtlist) ->
            try
                exists_types typetab id
            with
                Not_found -> 
                    let entry = {name=String.uppercase id; actual=String.uppercase id; 
                    evals=List.empty; members=NameMap.empty} in
                        NameMap.add entry.name entry typetab  

and walk_decl_1 typetab = function
      | Ast.GlobalType(type_decl) -> typewalk_1 typetab type_decl
      | Ast.GlobalFunc(func_decl) -> funcwalk_1 typetab func_decl

and walk_decl_2 typetab = function
    | Ast.GlobalType(type_decl) -> typewalk_2 typetab type_decl
    | Ast.GlobalFunc(func_decl) -> funcwalk_2 typetab func_decl

and args_add typetabeval = function
    | Ast.VarDecl(name, typename) ->
        try
            NameMap.add name typename typetabeval
        with
            Not_found -> raise (TypeNotDefined typename)

and funcwalk_2 typetab = function
    | Ast.FuncDecl(id, arglist, stmtlist) ->  
        List.map (fun a -> args_add (NameMap.find id typetab) a) arglist

and typewalk_2 typetab = function
    | Ast.TypeDef(id, type_element) ->
        List.map (fun te -> memvarwalk_2 typetab te) type_element

and memvarwalk_2 typetab = function
    | Ast.MemVarDecl(vardecl) -> varwalk_2 typetab vardecl

and varwalk_2 typetab = function
    | Ast.VarDecl(name, typename) ->  
        try 
            exists_types typetab typename
        with
            Not_found -> raise (TypeNotDefined typename)

in List.map (fun global_ele -> walk_decl_1 typetab global_ele) program

exception VariableNotDefined of string
exception TypeNotDefined of string
exception SemanticError of string

module NameMap = Map.Make(String)

type type_entry =  { 
  name: string; (* type name used in yo *)
  actual: string; (* actual name used in target language *)
  members: type_entry NameMap.t (* map of member => type_entry *)
  }
  
type var_entry = {
  name: string; (* type name used in yo *)
  actual: string; (* actual name used in target language *)
  type_def: type_entry (* type definition *)
  }

(* compile environment: variable symbol table * type environment table *)
type compile_context = {
  mutable vsymtab : var_entry NameMap.t list; (* a stack of variable symbol maps of varname => var_entry *)
  mutable typetab: type_entry NameMap.t (* type environment table: a map of typename => type *)
}
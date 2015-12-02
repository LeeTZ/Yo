type op = Add | Sub | Mult | Div | Mod | Eq | Neq | Less | Leq | Gt | Geq | And | Or

type types = Int | Double | Bool | String | Array of types

type expr =                                        (* Expressions*)
    IntConst of int                                (* 35 *)
  | DoubleConst of float                           (* 21.4 *)
  | BoolConst of bool                              (* True *)
  | StrConst of string                             (* "ocaml" *)
  | ArrayConst of expr list                        (* [12,23,34,56] *)
  | ArrayIndex of expr * expr                      (* A[B[3]]*)
	| Var of string                                  (* foo *)  
  | DotExpr of expr * string                       (* A.B *)
  | Call of expr option * string * expr list       (* foo(a, b) *)
	| Binop of expr * op * expr
	| NewArray of string														 (* Int[] *)
  | Noexpr

type stmt =
  | Assign of expr option * expr
  | IfStmt of cond_exec list
  | ForIn of string * expr * stmt list
  | ForRange of string * expr * expr * stmt list * for_range_dir
  | WhileStmt of expr * stmt list
  | Continue 
  | Break 
  | Return of expr option

and for_range_dir = Inc | Dec

and cond_exec = 
   CondExec of expr option * stmt list

and var_decl = 
  | VarDecl of string * string

and func_decl = 
  | FuncDecl of string * var_decl list * stmt list

and type_decl = 
  | TypeDecl of string * mem_type_decl list

and mem_type_decl = 
  | MemVarDecl of var_decl
  | MemFuncDecl of func_decl
  | MemTypeDecl of type_decl

and global_ele_decl = 
  | GlobalStmt of stmt
  | GlobalFunc of func_decl
  | GlobalType of type_decl

type program = global_ele_decl list

let string_of_op = function
	| Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | Mod -> "%"
  | Eq -> "==" | Neq -> "!=" | Less -> "<" | Leq -> "<=" | Gt -> ">" | Geq -> ">=" 
	| And -> "&&" | Or -> "||"

let rec string_of_expr = function
  | IntConst l -> string_of_int l
  | DoubleConst d -> string_of_float d 
  | BoolConst b -> string_of_bool b
  | StrConst s -> s
  | Var v -> v
	| NewArray s -> "Array<" ^ s ^ ">"
  | ArrayIndex(a, b) -> (string_of_expr a) ^ "[" ^ (string_of_expr b) ^ "]"
	| ArrayConst(e) -> let s = (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.map string_of_expr e)) in 
  "[" ^ (String.sub s 2 ((String.length s) - 2))  ^ "]"
  | DotExpr(a, b) -> (string_of_expr a) ^ "." ^ b
  | Binop(e1, o, e2) -> (string_of_expr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_expr e2)
  | Call(obj, f, el) -> (match obj with 
					| None -> "" | Some s -> (string_of_expr s) ^ "." )^ f ^ "(" ^ (String.concat ", " (List.map string_of_expr el)) ^ ")"
  | Noexpr -> ""
and string_of_stmt = function
  | Assign(None,rvalue) -> string_of_expr rvalue
  | Assign(Some(lvalue), rvalue) -> (string_of_expr lvalue) ^ " = " ^ (string_of_expr rvalue)
  | IfStmt(conds) -> string_of_first_cond_exec (List.hd conds) ^ "\n" ^
  (String.concat "\n" (List.map string_of_cond_exec (List.tl conds)))
  | ForIn(var, expr, stmts) -> "for " ^ var ^ " in " ^ (string_of_expr expr) 
    ^ ":\n" ^ (String.concat "\n" (List.map string_of_stmt stmts))
  | ForRange(var, exprst, expred, stmts, dir) -> "for " ^ var ^ " = " ^ (string_of_expr exprst) 
    ^ (match dir with | Inc -> " to " | Dec -> " downto ") ^ (string_of_expr expred) ^ ":\n" ^(String.concat "\n" (List.map string_of_stmt stmts))
  | WhileStmt(expr, stmts) -> "while " ^ (string_of_expr expr) ^ ":\n" 
    ^ (String.concat "\n" (List.map string_of_stmt stmts))
  | Continue -> "continue"
  | Break -> "break"
  | Return(None) -> "return"
  | Return(Some(expr)) -> "return " ^ (string_of_expr expr)

and string_of_first_cond_exec = function
  | CondExec(None, stmts) -> "else:" ^ (String.concat "\n" (List.map string_of_stmt stmts))
  | CondExec(Some(expr), stmts) -> "if " ^ (string_of_expr expr) ^ ":\n" ^ (String.concat "\n" (List.map string_of_stmt stmts))

and string_of_cond_exec = function
  | CondExec(None, stmts) -> "else:" ^ (String.concat "\n" (List.map string_of_stmt stmts))
  | CondExec(Some(expr), stmts) -> "elif " ^ (string_of_expr expr) ^ ":\n" ^ (String.concat "\n" (List.map string_of_stmt stmts))

and string_of_var_decl = function
  | VarDecl(ty, id) -> ty ^ " " ^ id

and string_of_func_decl = function
  | FuncDecl(name, args, stmts) -> "func " ^ name ^ " (" ^ (String.concat ", " (List.map string_of_var_decl args)) 
    ^ ")\n" ^ (String.concat "\n" (List.map string_of_stmt stmts))

and string_of_type_decl = function
  | TypeDecl(name, args) -> "type " ^ name ^ "\n" ^ (String.concat "\n" (List.map string_of_type_mem_decl args))

and string_of_type_mem_decl = function
  | MemVarDecl o -> string_of_var_decl o
  | MemFuncDecl o -> string_of_func_decl o 
  | MemTypeDecl o -> string_of_type_decl o

and string_of_global_ele_decl = function
  | GlobalStmt o -> string_of_stmt o
  | GlobalFunc o -> string_of_func_decl o
  | GlobalType o -> string_of_type_decl o

and string_of_program program =
  String.concat "\n" (List.map string_of_global_ele_decl program) 

  (*List.iter 
  fun global_ele -> match global_ele with
  | FuncDecl 
  | TypeDecl
  | Stmt
  program*) 


exception VariableNotDefined of string
exception TypeNotDefined of string
exception SemanticError of string
exception TypeExist of string

module NameMap = Map.Make(String)
module MemberMap = Map.Make(String)

type eval_entry = {
    mutable args: var_entry list;
    mutable ret: type_entry
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

let binop_type_tab = function
	| Add -> "$add"
	| Sub -> "$sub"
	| Mult -> "$mult"
	| Div -> "$div"
	| Mod -> "$mod"
	| Eq -> "$equal"
	| Neq -> "$neq"
	| Less -> "$less"
	| Leq -> "$leq"
	| Gt -> "$gt"
	| Geq -> "$geq"
	| And -> "$and"
	| Or -> "$or"
type op = Add | Sub | Mult | Div | Mod | Eq | Neq | Less | Leq | Gt | Geq | And | Or

type types = Int | Double | Bool | String  of types

type type_name = 
  | SimpleType of string
  | NestedType of type_name * string
  | ArrayType of type_name

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
  | ArrayRange of expr * expr * expr
  | ClipCascade of expr * expr * expr
  | ClipPixel of expr * coord * expr
  | ClipConcat of expr * expr
  | BuildArray of array_constructor * expr list
and array_constructor = 
  | SimpleArrayConstructor of expr
  | CompositeArrayConstructor of array_constructor
and coord = 
  | Coord of expr * expr

type stmt =
  | Assign of expr option * expr
  | SetAttribute of expr * expr * expr
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
  | VarDecl of string * type_name

and func_decl = 
  | FuncDecl of string * var_decl list * type_name * stmt list

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

let rec string_of_type_name = function
  | SimpleType t -> t
  | NestedType (p, t) -> (string_of_type_name p) ^ "." ^ t
  | ArrayType t -> (string_of_type_name t) ^ "[]"

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
  | ArrayIndex(a, b) -> (string_of_expr a) ^ "[" ^ (string_of_expr b) ^ "]"
  | ArrayConst(e) -> let s = (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.map string_of_expr e)) in 
  "[" ^ (String.sub s 2 ((String.length s) - 2))  ^ "]"
  | DotExpr(a, b) -> (string_of_expr a) ^ "." ^ b
  | Binop(e1, o, e2) -> (string_of_expr e1) ^ " " ^ (string_of_op o) ^ " " ^ (string_of_expr e2)
  | Call(obj, f, el) -> (match obj with 
          | None -> "" | Some s -> (string_of_expr s) ^ "." )^ f ^ "(" ^ (String.concat ", " (List.map string_of_expr el)) ^ ")"
  | ArrayRange (cl, st, ed) -> (string_of_expr cl) ^ "[" ^ (string_of_expr st) ^ ":" ^ (string_of_expr ed) ^ "]"
  | ClipCascade (cl1, cl2, tm) -> (string_of_expr cl1) ^ "^" ^ (string_of_expr cl2) ^ "@" ^ (string_of_expr tm)
  | ClipConcat (cl1, cl2) -> (string_of_expr cl1) ^ "&" ^ (string_of_expr cl2)
  | ClipPixel (cl, c, tm) -> (string_of_expr cl) ^ (string_of_coord c) ^ (string_of_expr tm)
  | BuildArray (t, el) -> (string_of_array_constructor t) ^ "(" ^ (String.concat ", " (List.map string_of_expr el)) ^ ")"

and string_of_coord = function
  | Coord (x, y) -> "<" ^ (string_of_expr x) ^ ", " ^ (string_of_expr y) ^ ">"

and string_of_array_constructor = function
  | SimpleArrayConstructor e -> (string_of_expr e) ^ "[]"
  | CompositeArrayConstructor e -> (string_of_array_constructor e) ^ "[]"

and string_of_stmt = function
  | Assign(None,rvalue) -> string_of_expr rvalue
  | Assign(Some(lvalue), rvalue) -> (string_of_expr lvalue) ^ " = " ^ (string_of_expr rvalue)
  | SetAttribute(main, time, value) -> (string_of_expr main) ^ "@" ^ (string_of_expr time) ^ "=" ^ (string_of_expr value)
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
  | VarDecl(id, ty) -> (string_of_type_name ty) ^ " " ^ id

and string_of_func_decl = function
  | FuncDecl(name, args, retype, stmts) -> "func " ^ name ^ " (" ^ (String.concat ", " (List.map string_of_var_decl args)) 
    ^ ")->" ^ (string_of_type_name retype) ^ "\n" ^ (String.concat "\n" (List.map string_of_stmt stmts))

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
exception ProcessingError of string
exception TypeExist of string
exception GenerationError of string
exception TypeRedefined of string

module NameMap = Map.Make(String)


type eval_entry = {
    mutable args: var_entry list;
    mutable ret: type_entry
    }
and base_type =  { 
  t_name: string; (* type name used in yo *)
  t_actual: string; (* actual name used in target language *)
  mutable evals: eval_entry list; (* a list of eval functions *)
  mutable members: type_entry NameMap.t (* map of member_name => type_entry *)
  }
and type_entry =  BaseTypeEntry of base_type | ArrayTypeEntry of type_entry
and var_entry = {
  v_name: string; (* type name used in yo *)
  v_actual: string; (* actual name used in target language *)
  v_type: type_entry (* type definition *)
  }

let rec compare_type t1 t2 = match t1, t2 with 
  | BaseTypeEntry _, ArrayTypeEntry _ | ArrayTypeEntry _,  BaseTypeEntry _ -> false
  | ArrayTypeEntry a, ArrayTypeEntry b -> compare_type a b
  | BaseTypeEntry a, BaseTypeEntry b -> a.t_name = b.t_name && a.t_actual = b.t_actual

(* compile environment: variable symbol table * type environment table *)
type compile_context = {
  mutable vsymtab: var_entry NameMap.t list; (* a stack of variable symbol maps of varname => var_entry *)
  mutable typetab: base_type NameMap.t (* type environment table: a map of base type name => base_type *)
}
(*
let base_type ctx type_name = BaseTypeEntry(look_up_type type_name ctx.typetab) 
*)
let binop_type_tab = function
  | Add   -> "__Add"
  | Sub   -> "__Sub"
  | Mult  -> "__Mult"
  | Div   -> "__Div"
  | Mod   -> "__Mod"
  | Eq    -> "__Equal"
  | Neq   -> "__Neq"
  | Less  -> "__Less"
  | Leq   -> "__Leq"
  | Gt    -> "__Gt"
  | Geq   -> "__Geq"
  | And   -> "__And"
  | Or    -> "__Or"
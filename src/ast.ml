type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type types = Int | Double | Bool | String | Array of types

type expr =                                        (* Expressions*)
    IntConst of int                                (* 35 *)
  | DoubleConst of float                           (* 21.4 *)
  | BoolConst of bool                              (* True *)
  | StrConst of string                             (* "ocaml" *)
  | ArrayConst of expr list                        (* [12,23,34,56] *)
  | ArrayExpr of expr * expr                       (* A[B[3]]*)
	| Var of string                                  (* foo *)  
  | DotExpr of expr * string                       (* A.B *)
  | Call of expr option * string * expr list       (* foo(a, b) *)
	| Binop of expr * op * expr
  | Noexpr

type stmt =
  | Assign of expr option * expr
  | IfStmt of cond_exec list
  | ForIn of string * expr * stmt list
  | ForEq of string * expr * expr * stmt list
  | WhileStmt of expr * stmt list
  | Continue 
  | Break 
  | Return of expr option

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

let rec string_of_expr = function
  | IntConst l -> string_of_int l
  | DoubleConst d -> string_of_float d 
  | BoolConst b -> string_of_bool b
  | StrConst s -> s
  | Var v -> v
  | ArrayExpr(a, b) -> (string_of_expr a) ^ "[" ^ (string_of_expr b) ^ "]"
	| ArrayConst(e) -> let s = (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.map string_of_expr e)) in 
  "[" ^ (String.sub s 2 ((String.length s) - 2))  ^ "]"
  | DotExpr(a, b) -> (string_of_expr a) ^ "." ^ b
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^
      (match o with | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!=" | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" | And -> "&&" | Or -> "||") 
			^ " " ^ string_of_expr e2
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
  | ForEq(var, exprst, expred, stmts) -> "for " ^ var ^ " = " ^ (string_of_expr exprst) 
    ^ " to " ^ (string_of_expr expred) ^ ":\n" ^(String.concat "\n" (List.map string_of_stmt stmts))
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
  | VarDecl(ty, id) -> ty ^ " " ^ id ^ "\n"

and string_of_func_decl = function
  | FuncDecl(name, args, stmts) -> "func " ^ name ^ " (" ^ (String.concat ", " (List.map string_of_var_decl args)) 
    ^ ") " ^ (String.concat "\n" (List.map string_of_stmt stmts))

and string_of_type_decl = function
  | TypeDecl(name, args) -> "type " ^ name ^ " " ^ (String.concat ", " (List.map string_of_type_mem_decl args))

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


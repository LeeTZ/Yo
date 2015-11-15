type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or

type types = Int | Double | Bool | String | Array of types

type expr =                                 (* Expressions*)
    IntConst of int                         (* 35 *)
  | DoubleConst of float                    (* 21.4 *)
  | BoolConst of bool                       (* True *)
  | StrConst of string                      (* "ocaml" *)
  | ArrayConst of expr list                 (* [12,23,34,56] *)
  | ArrayExpr of expr * expr                (* A[B[3]]*)
	| Var of string              (* foo *)  
  | DotExpr of expr * string        (* A.B *)
  | Call of expr option * string * expr list       (* foo(a, b) *)
	| Binop of expr * op * expr

type stmt =
  | Assign of expr option * expr
  | IfStmt of cond_exec list
  | ForIn of expr * stmt list
  | ForEq of expr * expr * stmt list
  | WhileStmt of expr * stmt list
  | Continue 
  | Break 
  | Return of expr option

type cond_exec = 
   CondExec of expr option * stmt list
	
	
type var_decl = 
	| VarDecl of string * string

type type_mem_decl = 
	| VarDecl of string * string
	| FuncDecl of string * var_decl list * stmt list
	|	TypeDecl of string * type_mem_decl list
 
type program = 
	| Program of type_mem_decl list

let rec string_of_expr = function
  | IntConst l -> string_of_int l
  | DoubleConst d -> string_of_float d 
  | BoolConst b -> string_of_bool b
  | StrConst s -> s
  | Var v -> v
  | ArrayExpr(a, b) -> (string_of_expr a) ^ "[" ^ (string_of_expr b) ^ "]"
	| ArrayConst(e) -> "[ " ^ (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.map string_of_expr e))  ^ "]"
  | DotExpr(a, b) -> (string_of_expr a) ^ "." ^ b
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^
      (match o with | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!=" | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=" | And -> "&&" | Or -> "||") 
			^ " " ^ string_of_expr e2
  | Call(obj, f, el) -> (match obj with 
					| None -> "" | Some s -> (string_of_expr s) ^ "." )^ f ^ "(" ^ (String.concat ", " (List.map string_of_expr el)) ^ ")"


let string_of_type_mem_decl = function
  | VarDecl(ty, id) -> ty ^ " " ^ id ^ "\n"
  | FuncDecl(name, args, stmts) -> "func " ^ name ^ " (" ^ (String.concat ", " (List.map string_of_var_decl args)) ^ ") " ^ (String.concat "\n" (List.map string_of_stmt stmts))
  | TypeDecl(name, args) -> "type " ^ name ^ " " ^ (String.concat ", " (List.map string_of_type_mem_decl args))

let string_of_program type_mem_decls =
  String.concat "" (List.map string_of_type_mem_decl type_mem_decls)

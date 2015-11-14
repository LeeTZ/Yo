type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

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
  | Noexpr

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
   CondExec of expr option * stmt

type val_decl = 
	| ValueDecl of string * string

type type_mem_decl = 
	| ValueDecl of string * string
	| FuncDecl of string * val_decl list * stmt list
	|	TypeDecl of string * type_mem_decl list
 
type program = 
	| Program of type_mem_decl list
	 
let rec string_of_expr = function
    IntConst(l) -> string_of_int l
  | DoubleConst(d) -> string_of_float d 
  | BoolConst(b) -> string_of_bool b
  | StrConst(s) -> s
  | Var(s) -> s
  | ArrayExpr(a, b) -> (string_of_expr a) ^ "[" ^ (string_of_expr b) ^ "]"
	| ArrayConst(e) -> "[ " ^ (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.map string_of_expr e))  ^ "]"
  | DotExpr(a, b) -> (string_of_expr a) ^ "." ^ b
  | Binop(o, e1, e2) -> string_of_expr e1 ^ " " ^
      (match o with | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!=" | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") 
			^ " " ^ string_of_expr e2
  | Call(f, el) -> f ^ "(" ^ (String.concat ", " (List.map string_of_expr el)) ^ ")"
  | Noexpr -> ""





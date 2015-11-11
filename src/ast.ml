type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =                                 (* Expressions*)
    IntConst of int                         (* 35 *)
  | DoubleConst of float                    (* 21.4 *)
  | BoolConst of bool                       (* True *)
  | StrConst of string                      (* "ocaml" *)
  | ArrayConst of expr list                 (* [12,23,34,56] *)
  | Id of string                            (* foo *)  
  | Array of expr * expr
  | DotExpr of expr * string
  | Binop of expr * op * expr
  | Call of string * fargs list
  | Log of string
  | PrimaryExpr of primary_expr             (* A.B, A[3] *)
  | Binop of expr * op * expr               (* 3+4 *)
  | Assign of primary_expr * expr           (* a = 3 *)
  | Call of string * args list              (* foo(a, b) *)
  | LogStmt of expr                         (* log a+b *)
  | Noexpr

type primary_expr = 
    ArrayExpr of expr * expr                (* A[B[3]]*)
  | DotExpr of expr * string                (* A.B *)

type args = 
  args of expr * expr

type stmt =
    BraceStmt of stmt list
  | Assign of primary_expr * expr
  | Expr of expr
  | LogStmt of expr
  | IfStmt of else_stmt * elif_stmt * cond_exec
  | ForIn of expr * stmt
  | ForEq of expr * expr * stmt
  | WhileStmt of expr * stmt
  | CONTINUE 
  | BREAK 
  | Return of expr

type elif_stmt = 
   ElifStmt of elif_stmt * cond_exec

type else_stmt = 
   ElseStmt of cond_exec_fallback

type cond_exec = 
   CondExec of stmt * stmt list

type cond_exec_fallback = 
   CondExecFallback of stmt list
   

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }


type program = string list * func_decl list
  

let rec string_of_primary_expr = function
    Id(s) -> s
  | ArrayExpr(a, b) -> (string_of_primary_expr a) ^ "[" ^ (string_of_expr b) ^ "]"
  | DotExpr(a, b) -> (string_of_primary_expr a) ^ "." ^ b

let rec string_of_expr = function
    IntConst(l) -> string_of_int l
  | DoubleConst(d) -> string_of_double d 
  | BoolConst(b) -> string_of_bool b
  | StrConst(s) -> s
  | Id(s) -> s
  | PrimaryExpr(a) -> string_of_primary_expr a
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^
      (match o with
	       Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/"
      | Equal -> "==" | Neq -> "!="
      | Less -> "<" | Leq -> "<=" | Greater -> ">" | Geq -> ">=") ^ " " ^
      string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Log(e) -> "log " ^ (string_of_expr e)
  | Noexpr -> ""





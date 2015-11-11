type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq

type expr =                                 (* Expressions*)
    IntConst of int                         (* 35 *)
  | DoubleConst of float                    (* 21.4 *)
  | BoolConst of bool                       (* True *)
  | StrConst of string                      (* "ocaml" *)
  | ArrayConst of expr list                 (* [12,23,34,56] *)
  | Id of string                            (* foo *)  
  | Array of expr * expr
  | Dot_Expr of expr * string
  | Binop of expr * op * expr
  | Call of string * fargs list
  | Log of string
  | Noexpr

and dec =                                   (* Declarations *)
    Tysig of string * types list            (* f :: Int -> [Note] -> Bool *)
    | Funcdec of func_decl                  (* f x y = x + y *)
    | Vardef of string * expr               (* x = (2 + 5) : [1,2,3] *)
    | Main of expr                          (* main (f x) + (g x) *)

and func_decl = {                      (* Function Declaration *)
    fname : string;                         (* Function name *)
    args : pattern list;                    (* Pattern arguments *)
    value : expr;                           (* Expression bound to function *)
}

and pattern =                          (* Patterns *)
    Patconst of int                         (* integer *)
    | Patbool of bool                       (* boolean *)
    | Patvar of string                      (* identifier*)
    | Patwild                               (* wildcard *)
    | Patcomma of pattern list              (* [pattern, pattern, pattern, ... ] or [] *)
    | Patcons of pattern * pattern          (* pattern : pattern *)

and fargs =                                 (* Function Arguments *)
      Arglit of int                         (* 42 *)
    | Argbool of bool                       (* True *)
    | Argvar of string                      (* bar *)
    | Argbeat of expr * int                 (* 2. *)
    | Argnote of  expr * expr * expr        (* (11, 2)^4. *)
    | Argchord of expr list                 (* [(11,3)$4., (5,2)$4.] *)
    | Argsystem of expr list                (* [ [(11,3)$4.,(5,2)$4.], [(-1,0)$2] ] *)
    | Arglist of expr list                  (* [farg, farg, farg, ... ] or [] *)
    | Argparens of expr                     (* parenthesized expressions *)

type program = dec list                     (* A program is a list of declarations *)



type stmt =
    Brace_Stmt of stmt list
  | Assign of primary_expr * expr
  | Expr of expr
  | Log_Stmt of expr
  | If_Stmt of elif_stmt * else_stmt * expr * stmt list
  | For_In of expr * stmt
  | For_Eq of expr * expr * stmt
  | While_Stmt of expr * stmt
  | CONTINUE 
  | BREAK 
  | Return of expr

type func_decl = {
    fname : string;
    formals : string list;
    locals : string list;
    body : stmt list;
  }


type program = string list * func_decl list

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Id(s) -> s
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
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_vdecl id = "int " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat ", " fdecl.formals ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

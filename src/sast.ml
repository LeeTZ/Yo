open Ast

type action = 
  | NewVar
	| NewArr

let string_of_action = function
  | NewVar -> "new"
	| NewArr -> "new array"



type sem = {
  mutable actions: action list;
  type_def: type_entry
}

let rec string_of_type = function
  | BaseTypeEntry t -> t.t_name
  | ArrayTypeEntry t -> (string_of_type t) ^ "[]"

let string_of_sem s = "$" ^ (string_of_type s.type_def) ^ " " 
                      ^ (String.concat " | " (List.map string_of_action s.actions)) ^ "$"

type s_expr =                                 (* Expressions*)
  | SLiteral of string * sem      (* int, double, bool, string *)
  | SArrayLiteral of s_expr list * sem
  | SArrayIndex of s_expr * s_expr * sem               (* A[B[3]]*)
  | SArrayRange of s_expr * s_expr * s_expr * sem
  | SVar of string * sem             (* foo *)
  | SDotExpr of s_expr * string * sem        (* A.B *)
  | SBinop of s_expr * op * s_expr * sem      (* 3+4 *)
  | SCall of s_expr option * base_type * s_expr list * sem      (* foo(a, b) *)
  | SBuildArray of type_entry * s_expr list * sem
  | SClipTimeIndex of s_expr * s_expr * sem
  | SClipFrameIndex of s_expr * s_expr * sem
  | SClipTimeRange of s_expr * s_expr * s_expr * sem
  | SClipFrameRange of s_expr * s_expr * s_expr * sem
  | SClipConcat of s_expr * s_expr * sem
  | SClipTimeCascade of s_expr * s_expr * s_expr * sem
  | SClipFrameCascade of s_expr * s_expr * s_expr * sem
  | SClipPixel of s_expr * s_expr * s_expr * s_expr * sem
  
type s_stmt =
  | SAssign of s_expr option * s_expr
  | STimeSetAttribute of s_expr * string * s_expr * s_expr
  | SFrameSetAttribute of s_expr * string * s_expr * s_expr
  | SIfStmt of s_cond_exec list
  | SForIn of string * sem * s_expr * s_stmt list
  | SForRange of string * sem * s_expr * s_expr * s_stmt list * for_range_dir
  | SWhileStmt of s_expr * s_stmt list
  | SContinue 
  | SBreak 
  | SReturn of s_expr option

and s_cond_exec = 
   SCondExec of s_expr option * s_stmt list


type s_var_decl = 
  | SVarDecl of string * sem

and s_func_decl = 
  | SFuncDecl of string * s_var_decl list * s_stmt list * sem

and s_type_decl = 
  | STypeDecl of string * s_type_mem_decl list

and s_type_mem_decl = 
  | SMemVarDecl of s_var_decl
  | SMemFuncDecl of s_func_decl
  | SMemTypeDecl of s_type_decl

and s_global_ele_decl = 
  | SGlobalStmt of s_stmt
  | SGlobalFunc of s_func_decl
  | SGlobalType of s_type_decl

type s_program = s_global_ele_decl list

let rec string_of_s_expr = function
  | SLiteral (str, s) -> str ^ (string_of_sem s)
  | SArrayLiteral (selist, sem) -> let s = (List.fold_left (fun a b -> a ^ ", " ^ b) "" (List.map string_of_s_expr selist)) in 
  "[" ^ (String.sub s 2 ((String.length s) - 2))  ^ "]" ^ (string_of_sem sem)
  | SArrayIndex (sout, sin, s) -> (string_of_s_expr sout) ^ "[" ^ (string_of_s_expr sin) ^ "]" ^ (string_of_sem s)
	| SArrayRange (arr, st, ed, s) -> (string_of_s_expr arr) ^ "[" ^ (string_of_s_expr st) ^ "," ^ (string_of_s_expr ed) ^ "]" ^ (string_of_sem s)
  | SVar (id, s) -> id ^ (string_of_sem s)
  | SDotExpr (sexpr, id, s) -> (string_of_s_expr sexpr) ^ "." ^ id ^ (string_of_sem s)
  | SBinop (lsexpr, op, rsexpr, s) -> (string_of_s_expr lsexpr) ^ " " ^ (string_of_op op) ^ " " ^ (string_of_s_expr rsexpr) ^ (string_of_sem s)
  | SCall (obj, func_type, el, s) -> (match obj with 
          | None -> "" | Some st -> (string_of_s_expr st) ^ "." ) ^ func_type.t_name ^ "(" ^ (String.concat ", " (List.map string_of_s_expr el)) ^ ")" ^ (string_of_sem s)
  | SClipTimeRange (cl, st, ed, s) -> (string_of_s_expr cl) ^ "[" ^ (string_of_s_expr st) ^ "," ^ (string_of_s_expr ed) ^ "]" ^ (string_of_sem s)
  | SClipFrameRange (cl, st, ed, s) -> (string_of_s_expr cl) ^ "[" ^ (string_of_s_expr st) ^ "," ^ (string_of_s_expr ed) ^ "]" ^ (string_of_sem s)
  | SClipTimeCascade (cl1, cl2, tm, s) -> (string_of_s_expr cl1) ^ "^" ^ (string_of_s_expr cl2) ^ "@" ^ (string_of_s_expr tm) ^ (string_of_sem s)
  | SClipFrameCascade (cl1, cl2, tm, s) -> (string_of_s_expr cl1) ^ "^" ^ (string_of_s_expr cl2) ^ "@" ^ (string_of_s_expr tm) ^ (string_of_sem s)
  | SClipConcat (cl1, cl2, s) -> (string_of_s_expr cl1) ^ "&" ^ (string_of_s_expr cl2)  ^ (string_of_sem s)
  | SClipTimeIndex (cl, idx, s) -> (string_of_s_expr cl) ^ "[" ^ (string_of_s_expr idx) ^ "]" ^ (string_of_sem s)
  | SClipFrameIndex (cl, idx, s) -> (string_of_s_expr cl) ^ "[" ^ (string_of_s_expr idx) ^ "]" ^ (string_of_sem s)
  | SClipPixel (cl, x, y, tm, s) -> (string_of_s_expr cl) ^ "<" ^ (string_of_s_expr x) ^ (string_of_s_expr y) ^ ">" ^ "@" ^ (string_of_s_expr tm) ^ (string_of_sem s)
  | SBuildArray (t, el, s) -> (string_of_type t) ^ "(" ^ (String.concat ", " (List.map string_of_s_expr el)) ^ ")" ^ (string_of_sem s)

and string_of_s_stmt = function
  | SAssign(None,rvalue) -> string_of_s_expr rvalue
  | SAssign(Some(lvalue), rvalue) -> (string_of_s_expr lvalue) ^ " = " ^ (string_of_s_expr rvalue)
  | STimeSetAttribute(main, attr, time, value) -> (string_of_s_expr main) ^ attr ^ "@" ^ (string_of_s_expr time) ^ " = " ^ (string_of_s_expr value)
  | SFrameSetAttribute(main, attr, time, value) -> (string_of_s_expr main) ^ attr ^ "@" ^ (string_of_s_expr time) ^ " = " ^ (string_of_s_expr value)
  | SIfStmt(conds) -> string_of_s_first_cond_exec (List.hd conds) ^ "\n" ^
  (String.concat "\n" (List.map string_of_s_cond_exec (List.tl conds)))
  | SForIn(var, s, expr, stmts) -> "for " ^ var ^ (string_of_sem s) ^ " in " ^ (string_of_s_expr expr) 
    ^ ":\n" ^ (String.concat "\n" (List.map string_of_s_stmt stmts))
  | SForRange(var, s, exprst, expred, stmts, dir) -> "for " ^ var ^ (string_of_sem s) ^ " = " ^ (string_of_s_expr exprst) 
    ^ (match dir with | Inc -> " to " | Dec -> " downto ") ^ (string_of_s_expr expred) ^ ":\n" ^(String.concat "\n" (List.map string_of_s_stmt stmts))
  | SWhileStmt(expr, stmts) -> "while " ^ (string_of_s_expr expr) ^ ":\n" 
    ^ (String.concat "\n" (List.map string_of_s_stmt stmts))
  | SContinue -> "continue"
  | SBreak -> "break"
  | SReturn(None) -> "return"
  | SReturn(Some(expr)) -> "return " ^ (string_of_s_expr expr)

and string_of_s_first_cond_exec = function
  | SCondExec(None, stmts) -> "else:" ^ (String.concat "\n" (List.map string_of_s_stmt stmts))
  | SCondExec(Some(expr), stmts) -> "if " ^ (string_of_s_expr expr) ^ ":\n" ^ (String.concat "\n" (List.map string_of_s_stmt stmts))

and string_of_s_cond_exec = function
  | SCondExec(None, stmts) -> "else:" ^ (String.concat "\n" (List.map string_of_s_stmt stmts))
  | SCondExec(Some(expr), stmts) -> "elif " ^ (string_of_s_expr expr) ^ ":\n" ^ (String.concat "\n" (List.map string_of_s_stmt stmts))


let extract_semantic = function
  | SLiteral (_, s) -> s
  | SArrayLiteral (_, s) -> s
  | SArrayIndex (_, _, s) -> s  
  | SArrayRange (_, _, _, s) -> s            
  | SVar (_, s) -> s            
  | SDotExpr (_, _, s) -> s       
  | SBinop (_, _, _, s) -> s      
  | SCall (_, _, _, s) -> s
  | SClipTimeIndex (_, _, s) -> s
  | SClipFrameIndex (_, _, s) -> s
  | SClipTimeRange (_, _, _, s) -> s
  | SClipFrameRange (_, _, _, s) -> s  
  | SClipTimeCascade (_, _, _, s) -> s  
  | SClipFrameCascade (_, _, _, s) -> s  
  | SClipConcat (_, _, s) -> s  
  | SClipPixel (_,_,_,_,s) -> s
  | SBuildArray (_, _, s) -> s


let rec string_of_s_var_decl = function
  | SVarDecl(id, s) -> id ^ (string_of_sem s)

and string_of_s_func_decl = function
  | SFuncDecl(name, args, stmts, s) -> "func " ^ name ^ " (" ^ (String.concat ", " (List.map string_of_s_var_decl args)) 
    ^ ")\n" ^ (String.concat "\n" (List.map string_of_s_stmt stmts)) ^ (string_of_sem s)

and string_of_s_type_decl = function
  | STypeDecl(name, args) -> "type " ^ name ^ "\n" ^ (String.concat "\n" (List.map string_of_s_type_mem_decl args))

and string_of_s_type_mem_decl = function
  | SMemVarDecl o -> string_of_s_var_decl o
  | SMemFuncDecl o -> string_of_s_func_decl o 
  | SMemTypeDecl o -> string_of_s_type_decl o

and string_of_s_global_ele_decl = function
  | SGlobalStmt o -> string_of_s_stmt o
  | SGlobalFunc o -> string_of_s_func_decl o
  | SGlobalType o -> string_of_s_type_decl o

and string_of_s_program program =
  String.concat "\n" (List.map string_of_s_global_ele_decl program) 
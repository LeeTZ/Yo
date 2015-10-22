type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | LITERAL of (int)
  | ID of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 34 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* PLUS *);
  264 (* MINUS *);
  265 (* TIMES *);
  266 (* DIVIDE *);
  267 (* ASSIGN *);
  268 (* EQ *);
  269 (* NEQ *);
  270 (* LT *);
  271 (* LEQ *);
  272 (* GT *);
  273 (* GEQ *);
  274 (* RETURN *);
  275 (* IF *);
  276 (* ELSE *);
  277 (* FOR *);
  278 (* WHILE *);
  279 (* INT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  280 (* LITERAL *);
  281 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\005\000\005\000\008\000\
\008\000\006\000\006\000\003\000\007\000\007\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\011\000\011\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\012\000\012\000\
\013\000\013\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\008\000\000\000\001\000\001\000\
\003\000\000\000\002\000\003\000\000\000\002\000\002\000\003\000\
\003\000\005\000\007\000\009\000\005\000\000\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\004\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\043\000\000\000\000\000\000\000\001\000\003\000\
\004\000\000\000\000\000\012\000\008\000\000\000\000\000\000\000\
\000\000\010\000\009\000\000\000\011\000\000\000\000\000\013\000\
\005\000\000\000\000\000\000\000\000\000\024\000\000\000\014\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\038\000\017\000\016\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\028\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\037\000\000\000\000\000\000\000\
\021\000\000\000\000\000\000\000\019\000\000\000\000\000\020\000"

let yydgoto = "\002\000\
\003\000\004\000\008\000\009\000\014\000\020\000\022\000\015\000\
\032\000\033\000\058\000\061\000\062\000"

let yysindex = "\006\000\
\000\000\000\000\000\000\001\000\243\254\028\255\000\000\000\000\
\000\000\031\255\019\255\000\000\000\000\042\255\040\255\045\255\
\033\255\000\000\000\000\047\255\000\000\029\255\011\255\000\000\
\000\000\011\255\063\255\066\255\071\255\000\000\000\255\000\000\
\092\255\253\255\038\255\136\255\011\255\011\255\011\255\011\255\
\011\255\000\000\011\255\011\255\011\255\011\255\011\255\011\255\
\011\255\011\255\011\255\011\255\000\000\000\000\000\000\020\000\
\063\000\078\255\035\000\063\000\082\255\084\255\063\000\067\255\
\067\255\000\000\000\000\074\000\074\000\131\255\131\255\131\255\
\131\255\094\255\011\255\094\255\000\000\011\255\077\255\102\255\
\000\000\063\000\094\255\011\255\000\000\107\255\094\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\108\255\000\000\000\000\000\000\111\255\000\000\
\000\000\000\000\000\000\062\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\119\255\000\000\
\000\000\000\000\000\000\000\000\000\000\116\255\000\000\118\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\255\000\000\000\000\002\255\000\000\120\255\003\255\153\255\
\170\255\000\000\000\000\052\000\056\000\187\255\204\255\221\255\
\238\255\000\000\116\255\000\000\000\000\000\000\070\255\000\000\
\000\000\035\255\000\000\121\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\110\000\000\000\000\000\000\000\118\000\000\000\
\251\255\233\255\191\255\000\000\000\000"

let yytablesize = 347
let yytable = "\034\000\
\007\000\040\000\036\000\036\000\041\000\036\000\001\000\041\000\
\036\000\080\000\041\000\010\000\023\000\056\000\057\000\059\000\
\060\000\063\000\086\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\071\000\072\000\073\000\011\000\023\000\012\000\
\024\000\025\000\030\000\031\000\023\000\042\000\023\000\023\000\
\042\000\024\000\054\000\013\000\016\000\017\000\026\000\027\000\
\018\000\028\000\029\000\057\000\030\000\031\000\082\000\026\000\
\027\000\019\000\028\000\029\000\057\000\030\000\031\000\013\000\
\037\000\013\000\013\000\038\000\079\000\005\000\081\000\018\000\
\039\000\018\000\018\000\045\000\046\000\085\000\075\000\013\000\
\013\000\088\000\013\000\013\000\077\000\013\000\013\000\018\000\
\018\000\078\000\018\000\018\000\042\000\018\000\018\000\023\000\
\083\000\024\000\043\000\044\000\045\000\046\000\084\000\047\000\
\048\000\049\000\050\000\051\000\052\000\087\000\006\000\026\000\
\027\000\007\000\028\000\029\000\022\000\030\000\031\000\025\000\
\039\000\025\000\040\000\022\000\025\000\025\000\025\000\025\000\
\025\000\021\000\025\000\025\000\025\000\025\000\025\000\025\000\
\055\000\043\000\044\000\045\000\046\000\035\000\043\000\044\000\
\045\000\046\000\000\000\047\000\048\000\049\000\050\000\051\000\
\052\000\026\000\000\000\026\000\000\000\000\000\026\000\026\000\
\026\000\000\000\000\000\000\000\026\000\026\000\026\000\026\000\
\026\000\026\000\027\000\000\000\027\000\000\000\000\000\027\000\
\027\000\027\000\000\000\000\000\000\000\027\000\027\000\027\000\
\027\000\027\000\027\000\032\000\000\000\032\000\000\000\000\000\
\032\000\000\000\000\000\000\000\000\000\000\000\032\000\032\000\
\032\000\032\000\032\000\032\000\033\000\000\000\033\000\000\000\
\000\000\033\000\000\000\000\000\000\000\000\000\000\000\033\000\
\033\000\033\000\033\000\033\000\033\000\034\000\000\000\034\000\
\000\000\000\000\034\000\000\000\000\000\000\000\000\000\000\000\
\034\000\034\000\034\000\034\000\034\000\034\000\035\000\000\000\
\035\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\035\000\035\000\035\000\035\000\035\000\035\000\053\000\
\000\000\000\000\000\000\043\000\044\000\045\000\046\000\000\000\
\047\000\048\000\049\000\050\000\051\000\052\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\074\000\005\000\
\000\000\006\000\043\000\044\000\045\000\046\000\000\000\047\000\
\048\000\049\000\050\000\051\000\052\000\076\000\000\000\000\000\
\000\000\043\000\044\000\045\000\046\000\000\000\047\000\048\000\
\049\000\050\000\051\000\052\000\030\000\000\000\030\000\000\000\
\031\000\030\000\031\000\000\000\000\000\031\000\000\000\030\000\
\030\000\000\000\000\000\031\000\031\000\043\000\044\000\045\000\
\046\000\000\000\047\000\048\000\049\000\050\000\051\000\052\000\
\043\000\044\000\045\000\046\000\000\000\000\000\000\000\049\000\
\050\000\051\000\052\000"

let yycheck = "\023\000\
\000\000\002\001\026\000\001\001\003\001\003\001\001\000\006\001\
\006\001\075\000\011\001\025\001\002\001\037\000\038\000\039\000\
\040\000\041\000\084\000\043\000\044\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\002\001\002\001\001\001\
\004\001\005\001\024\001\025\001\001\001\003\001\003\001\002\001\
\006\001\004\001\005\001\025\001\003\001\006\001\018\001\019\001\
\004\001\021\001\022\001\075\000\024\001\025\001\078\000\018\001\
\019\001\025\001\021\001\022\001\084\000\024\001\025\001\002\001\
\002\001\004\001\005\001\002\001\074\000\023\001\076\000\002\001\
\002\001\004\001\005\001\009\001\010\001\083\000\001\001\018\001\
\019\001\087\000\021\001\022\001\003\001\024\001\025\001\018\001\
\019\001\006\001\021\001\022\001\001\001\024\001\025\001\002\001\
\020\001\004\001\007\001\008\001\009\001\010\001\001\001\012\001\
\013\001\014\001\015\001\016\001\017\001\003\001\003\001\018\001\
\019\001\003\001\021\001\022\001\001\001\024\001\025\001\001\001\
\003\001\003\001\003\001\003\001\006\001\007\001\008\001\009\001\
\010\001\020\000\012\001\013\001\014\001\015\001\016\001\017\001\
\001\001\007\001\008\001\009\001\010\001\024\000\007\001\008\001\
\009\001\010\001\255\255\012\001\013\001\014\001\015\001\016\001\
\017\001\001\001\255\255\003\001\255\255\255\255\006\001\007\001\
\008\001\255\255\255\255\255\255\012\001\013\001\014\001\015\001\
\016\001\017\001\001\001\255\255\003\001\255\255\255\255\006\001\
\007\001\008\001\255\255\255\255\255\255\012\001\013\001\014\001\
\015\001\016\001\017\001\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\255\255\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\001\001\255\255\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\001\001\255\255\003\001\
\255\255\255\255\006\001\255\255\255\255\255\255\255\255\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\003\001\
\255\255\255\255\255\255\007\001\008\001\009\001\010\001\255\255\
\012\001\013\001\014\001\015\001\016\001\017\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\003\001\023\001\
\255\255\025\001\007\001\008\001\009\001\010\001\255\255\012\001\
\013\001\014\001\015\001\016\001\017\001\003\001\255\255\255\255\
\255\255\007\001\008\001\009\001\010\001\255\255\012\001\013\001\
\014\001\015\001\016\001\017\001\001\001\255\255\003\001\255\255\
\001\001\006\001\003\001\255\255\255\255\006\001\255\255\012\001\
\013\001\255\255\255\255\012\001\013\001\007\001\008\001\009\001\
\010\001\255\255\012\001\013\001\014\001\015\001\016\001\017\001\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\014\001\
\015\001\016\001\017\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 25 "parser.mly"
            ( _1 )
# 262 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
                 ( [], [] )
# 268 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 29 "parser.mly"
               ( (_2 :: fst _1), snd _1 )
# 276 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 30 "parser.mly"
               ( fst _1, (_2 :: snd _1) )
# 284 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 34 "parser.mly"
     ( { fname = _1;
	 formals = _3;
	 locals = List.rev _6;
	 body = List.rev _7 } )
# 297 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
                  ( [] )
# 303 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 41 "parser.mly"
                  ( List.rev _1 )
# 310 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                         ( [_1] )
# 317 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
                         ( _3 :: _1 )
# 325 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                     ( [] )
# 331 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 49 "parser.mly"
                     ( _2 :: _1 )
# 339 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 52 "parser.mly"
               ( _2 )
# 346 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
                   ( [] )
# 352 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 56 "parser.mly"
                   ( _2 :: _1 )
# 360 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
              ( Expr(_1) )
# 367 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                     ( Return(_2) )
# 374 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 61 "parser.mly"
                            ( Block(List.rev _2) )
# 381 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 389 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 63 "parser.mly"
                                            ( If(_3, _5, _7) )
# 398 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr_opt) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr_opt) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "parser.mly"
     ( For(_3, _5, _7, _9) )
# 408 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 66 "parser.mly"
                                  ( While(_3, _5) )
# 416 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "parser.mly"
                  ( Noexpr )
# 422 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 70 "parser.mly"
                  ( _1 )
# 429 "parser.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 73 "parser.mly"
                     ( Literal(_1) )
# 436 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 74 "parser.mly"
                     ( Id(_1) )
# 443 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 75 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 451 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 76 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 459 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 77 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 467 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 78 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 475 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 79 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 483 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 491 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 499 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 82 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 507 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 83 "parser.mly"
                     ( Binop(_1, Greater,  _3) )
# 515 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 84 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 523 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 85 "parser.mly"
                     ( Assign(_1, _3) )
# 531 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 86 "parser.mly"
                                 ( Call(_1, _3) )
# 539 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 87 "parser.mly"
                       ( _2 )
# 546 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                  ( [] )
# 552 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 91 "parser.mly"
                  ( List.rev _1 )
# 559 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                            ( [_1] )
# 566 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                            ( _3 :: _1 )
# 574 "parser.ml"
               : 'actuals_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)

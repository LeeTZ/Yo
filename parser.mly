%{ open Ast %}

%token INDENT NEWLINE
%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA DOT TILDE QUOTATION COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD AND OR AMPERSAND EXCLAMATION
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE ELIF FOR WHILE IN CONTINUE BREAK
%token INT DOUBLE BOOL STRING ARRAY
%token LAMBDA FUNCTION GLOBAL TYPE EVAL
%token FRAME CLIP
%token RIGHTARROW LEFTARROW CASCADE
%token LOG READFRAME WRITEFRAME
%token <int> IntLITERAL 
%token <float> DoubleLITERAL
%token <string> StringLITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left CASCADE
%left AMPERSAND
%left OR
%left AND
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left UMINUS
%left DOT

%start program
%type <Ast.program> program

%%
constant:
    IntLITERAL                       { IntCon $1 }
  | DoubleLITERAL                    { DoubleCon $1 }
  | StringLITERAL                    { StrCon $1 } 
  | BoolLITERAL                      { BoolCon $1 }

primary_expr:
    ID                                               { Id $1 }
  | constant                                         { $1 }
  | LPAREN primary_expr RPAREN                       { Paren_Expr $2 }
  | primary_expr LPAREN arg_expr_list RPAREN         { Func_Call($1 , Some($3)) }
  | primary_expr LPAREN RPAREN                       { Func_Call($1, None) }
  | array_expr
  | primary_expr DOT ID                              { Dot_Expr($1,$3) }

array_expr:
  primary_expr LBRACKET array_expr_list RBRACKET     { Array($1,$3) }

arg_expr_list:
  expr                      { [$1] }
  | arg_expr_list COM expr  { $3 :: $1 }

array_expr_list:
  ?

expr:
    primary_expr                             { $1 }
  | expr PLUS   expr                         { Binop($1, Add,   $3) }
  | expr MINUS  expr                         { Binop($1, Sub,   $3) }
  | expr TIMES  expr                         { Binop($1, Mult,  $3) }
  | expr DIVIDE expr                         { Binop($1, Div,   $3) }
  | expr EQ     expr                         { Binop($1, Equal, $3) }
  | expr NEQ    expr                         { Binop($1, Neq,   $3) }
  | expr LT     expr                         { Binop($1, Less,  $3) }
  | expr LEQ    expr                         { Binop($1, Leq,   $3) }
  | expr GT     expr                         { Binop($1, Greater,  $3) }
  | expr GEQ    expr                         { Binop($1, Geq,   $3) }
  | primary_expr ASSIGN expr                 { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN             { Call($1, $3) }
  | LPAREN expr RPAREN                       { $2 }

statement:
    NEWLINE                                                                     { Stmt(None) }
  | expr NEWLINE                                                                { Stmt(Some($1)) }
  | LBRACE RBRACE NEWLINE                                                       { Brace_Stmt(None) }
  | LBRACE statement_list RBRACE                                                { Brace_Stmt(Some(List.rev $2)) }
  | LOG expr NEWLINE                                                            { Log($1) }
  | IF expr NEWLINE statement elif_statement                                    { If($2, $4, $5)}
  | IF expr NEWLINE statement elif_statement ELSE statement                     { If_else($2, $4, $5, $7) }
  | WHILE expr NEWLINE statement                                                { While($2, $4) }
  | FOR ID IN array_expr NEWLINE statement                                      { For_in($4, $6) }
  | FOR ID EQ expr TO expr NEWLINE statement                                    { For_eq($4, $6, $8)  }
  | CONTINUE NEWLINE                                                            { CONTINUE }
  | BREAK NEWLINE                                                               { BREAK }
  | RETURN NEWLINE                                                              { RETURN }
  | RETURN expr NEWLINE                                                         { Return($2) }

statement_list:
    /* nothing */
  | statement_list statement 

elif_statement:
    /* nothing */
  | ELIF statement elif_statement NEWLINE { Elif($2,$3) }


program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }

fdecl:
   FUNCTION ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $1;
	 formals = $3;
	 locals = List.rev $6;
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    ID                   { [$1] }
  | formal_list COMMA ID { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   INT ID SEMI { $2 }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr_opt SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }



actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }

%{ 
  open Ast
%}

%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA DOT TILDE QUOTATION COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD AND OR AMPERSAND EXCLAMATION
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE ELIF FOR WHILE IN TO CONTINUE BREAK
/*%token INT DOUBLE BOOL STRING ARRAY FRAME CLIP*/
%token FUNCTION TYPE /*EVAL*/
%token /*RIGHTARROW LEFTARROW*/ HAT AT
%token TRUE FALSE
%token <int> IntLITERAL 
%token <float> DoubleLITERAL
%token <string> StringLITERAL
%token <bool> BoolLITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left EQ NEQ
%left HAT AT
%left AMPERSAND
%left OR
%left AND
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left UMINUS
%left DOT


%start global
%type <Ast.program> global

%%
literal:
    IntLITERAL                              { IntConst $1 }
  | DoubleLITERAL                           { DoubleConst $1 }
  | StringLITERAL                           { StrConst $1 } 
  | BoolLITERAL                             { BoolConst $1 }
  | array_literal                           { $1  }

arg_expr_opt:
  /* nothing */                              { [] }
  | arg_expr_list                            { List.rev $1 }

arg_expr_list:
  | expr                                     { [$1] }
  | arg_expr_list COMMA expr                 { $3 :: $1 }

array_literal:
  LBRACKET arg_expr_opt RBRACKET            { ArrayConst $2 }

primary_expr:
    ID                                               { Var $1 }  
  | primary_expr LBRACKET expr RBRACKET              { ArrayExpr($1, $3) }
  | primary_expr DOT ID                              { DotExpr($1, $3) }

expr:
    primary_expr                             { $1 }
  | literal                                  { $1 }
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
	| expr AND		expr												 { Binop($1, And,   $3) }
	| expr OR			expr												 { Binop($1, Or,   $3) }
  | ID LPAREN arg_expr_opt RPAREN            { Call(None, $1, $3) }
  | primary_expr DOT ID LPAREN arg_expr_opt RPAREN { Call(Some($1), $3, $5) }
  | LPAREN expr RPAREN                       { $2 }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

statement:
   expr SEMI                                                             { Assign(None, $1) }
  | primary_expr ASSIGN expr SEMI                                        { Assign(Some($1), $3) }
  | IF expr COLON LBRACE statement_opt RBRACE elif_statement_list else_statement { IfStmt(List.rev ($8 @ $7 @ [ CondExec(Some($2), $5) ])) }
  | WHILE expr COLON LBRACE statement_opt  RBRACE                        { WhileStmt($2, $5) }
  | FOR ID IN for_in_expr COLON LBRACE  statement_opt RBRACE             { ForIn($2, $4, $7) }
  | FOR ID ASSIGN expr TO expr COLON LBRACE statement_opt RBRACE             { ForEq($2, $4, $6, $9)  }
  | CONTINUE SEMI                                                        { Continue }
  | BREAK SEMI                                                           { Break }
  | RETURN expr_opt SEMI                                                 { Return (Some($2)) }

for_in_expr:
   ID            {Var $1}
  | array_literal {$1}

statement_opt:
  /* nothing */ { [] }
  | statement_list { List.rev $1 }

statement_list:  
  | statement { [ $1 ] }
  | statement_list statement { $2 :: $1 }

elif_statement_list:
   /* nothing */ { [] }
  | ELIF expr COLON LBRACE statement_opt RBRACE elif_statement_list { $7 @ [ CondExec(Some($2), $5) ] }

else_statement:
	/* nothing */ { [] }
  | ELSE COLON LBRACE statement_opt RBRACE { [CondExec(None, $4)] }

var_decl:
  ID COLON ID  	{ VarDecl($1, $3) }

mem_var_decl:
  var_decl   { MemVarDecl($1) }

func_decl:
	FUNCTION ID LPAREN func_arg_opt RPAREN COLON LBRACE statement_opt RBRACE      {FuncDecl($2, $4, $8)}

mem_func_decl:
  func_decl { MemFuncDecl($1) }

global_func_decl:
  func_decl { GlobalFunc($1) }

func_arg_opt:
   /* nothing */    	{ [] }
	| func_arg_list 		{ List.rev $1 }

func_arg_list:
	| var_decl												{ [$1] }
  | func_arg_list COMMA var_decl    { $3 :: $1 }
	
type_decl:
  TYPE ID COLON LBRACE type_element_list RBRACE    {TypeDecl($2, $5)}

mem_type_decl:
  type_decl  {MemTypeDecl($1)}

global_type_decl:
  type_decl { GlobalType($1) }

global_statement:
  statement  { GlobalStmt($1) }

type_element_list:
  /*  nothing */  { [] }
  | mem_var_decl type_element_list { $1 :: $2 }
  | mem_func_decl type_element_list { $1 :: $2 }
  | mem_type_decl type_element_list { $1 :: $2 }

global_ele:
	| global_func_decl	{ $1 }
	| global_type_decl	{ $1 }
	| global_statement	{ $1 }

global_ele_list:
	| global_ele 						{ [$1] }
	| global_ele_list global_ele	{ $2 :: $1 }

global:
	global_ele_list	{ List.rev $1 }
	

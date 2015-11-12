%token INDENT NEWLINE
%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA DOT TILDE QUOTATION COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD AND OR AMPERSAND EXCLAMATION
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE ELIF FOR WHILE IN TO CONTINUE BREAK
%token INT DOUBLE BOOL STRING ARRAY
%token FUNC GLOBAL TYPE EVAL
%token FRAME CLIP
%token RIGHTARROW LEFTARROW HAT AT
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

%start play
%type <int> play

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
    ID                                               { Id $1 }  
  | primary_expr LBRACKET expr RBRACKET              { ArrayExpr($1, $3) }
  | primary_expr DOT ID                              { DotExpr($1, $3) }

expr:
    primary_expr                             { $1 }
  | literal                                  { $1 }
  | expr PLUS   expr                         { Binop(Add, $1, $3) }
  | expr MINUS  expr                         { Binop(Sub, $1, $3) }
  | expr TIMES  expr                         { Binop(Mult, $1, $3) }
  | expr DIVIDE expr                         { Binop(Div, $1, $3) }
  | expr EQ     expr                         { Binop(Equal, $1, $3) }
  | expr NEQ    expr                         { Binop(Neq, $1, $3) }
  | expr LT     expr                         { Binop(Less, $1, $3) }
  | expr LEQ    expr                         { Binop(Leq, $1, $3) }
  | expr GT     expr                         { Binop(Greater, $1, $3) }
  | expr GEQ    expr                         { Binop(Geq, $1, $3) }
  | ID LPAREN arg_expr_opt RPAREN          	 { Call($1, $3) }
  | LPAREN expr RPAREN                       { $2 }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

statement:
   expr NEWLINE                                                                 { Expr $1 }
  | primary_expr ASSIGN expr NEWLINE                                            { Assign($1, $3) }
  /*| LBRACE NEWLINE statement_list RBRACE NEWLINE                                { $3 }*/
  | IF expr COLON LBRACE NEWLINE statement_opt RBRACE NEWLINE elif_statement_list else_statement { IfStmt(List.rev ($10 @ $9 @ [ CondExec($2, $6) ])) }
  | WHILE expr COLON LBRACE NEWLINE statement_opt  RBRACE NEWLINE               { WhileStmt($2, $6) }
  | FOR ID IN for_in_expr COLON LBRACE NEWLINE statement_opt  RBRACE NEWLINE    { ForIn($2, $4, $8) }
  | FOR ID EQ expr TO expr COLON LBRACE NEWLINE statement_opt RBRACE NEWLINE  { ForEq($2, $4, $6, $10)  }
  | CONTINUE NEWLINE                                                            { Continue }
  | BREAK NEWLINE                                                               { Break }
  | RETURN expr_opt NEWLINE                                                     { Return $2 }

for_in_expr:
   ID            {Id $1}
  | array_literal {$1}

statement_opt:
    /* nothing */ { [] }
  | statement_list { List.rev $1 }

statement_list:
    NEWLINE     { [] }
  | statement { [ $1 ] }
  | statement_list statement { $2 :: $1 }

elif_statement_list:
   /* nothing */ { [] }
  | ELIF expr COLON LBRACE NEWLINE statement_opt RBRACE NEWLINE elif_statement_list { $9 @ [ CondExec($2, $6) ] }

else_statement:
	/* nothing */ { [] }
  | ELSE COLON LBRACE NEWLINE statement_opt RBRACE NEWLINE { [CondExec(None, $5)] }

var_decl:
  ID COLON ID  	{ ValueDecl($1, $3) }
	
func_decl:
	FUNC ID LBRACE func_arg_opt RBRACE COLON NEWLINE LBRACKET statement_opt RBRACKET       {FuncDecl($2, $4, $9)}

func_arg_opt:
   /* nothing */    	{ [] }
	| func_arg_list 		{ List.rev $1 }

func_arg_list:
	| var_decl												{ [$1] }
  | func_arg_list COMMA var_decl    { $3 :: $1 }
	
type_element_list:
  /*  nothing */  { [] }
  | var_decl 		NEWLINE type_element_list { $1 :: $3 }
  | func_decl 	NEWLINE type_element_list { $1 :: $3 }
  | type_decl 	NEWLINE type_element_list { $1 :: $3 }

type_decl:
  TYPE ID COLON NEWLINE LBRACE type_element_list RBRACE    {TypeDef($2, $6)}
	
player:
	| func_decl	{ $1 }
	| type_decl	{ $1 }
	| statement	{ $1 }

player_list:
	| player 						{ [$1] }
	| player_list NEWLINE player	{ $3 :: $1 }

play:
	player_list	{ List.rev $1 }
	
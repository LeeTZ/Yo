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
%token <bool> BoolLITERAL
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

%start primary_expr
%type <int> primary_expr

%%

constant:
    IntLITERAL                              { IntCon $1 }
  | DoubleLITERAL                           { DoubleCon $1 }
  | StringLITERAL                           { StrCon $1 } 
  | BoolLITERAL                             { BoolCon $1 }

primary_expr:
    ID                                       { Id $1 }  
  | primary_expr LBRACKET expr RBRACKET      { Array($1,$3) }
  | primary_expr DOT ID                      { Dot_Expr($1,$3) }

expr:
    primary_expr                             { $1 }
  | constant                                 { $1 }
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
  | ID LPAREN arg_expr_opt RPAREN            { Call($1, $3) }
  | LPAREN expr RPAREN                       { $2 }

arg_expr_opt:
  /* nothing */                              { [] }
  | arg_expr_list                            { List.rev $1 }

arg_expr_list:
    expr                                     { [$1] }
  | arg_expr_list COMMA expr                 { $3 :: $1 }
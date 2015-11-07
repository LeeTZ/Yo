%token INDENT NEWLINE
%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA DOT TILDE QUOTATION COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD AND OR AMPERSAND EXCLAMATION
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE ELIF FOR WHILE IN TO CONTINUE BREAK
%token INT DOUBLE BOOL STRING ARRAY
%token LAMBDA FUNCTION GLOBAL TYPE EVAL
%token FRAME CLIP
%token RIGHTARROW LEFTARROW CASCADE
%token LOG
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

%start statement_opt
%type <int> statement_opt

%%

constant:
    IntLITERAL                              { Int_Con $1 }
  | DoubleLITERAL                           { Double_Con $1 }
  | StringLITERAL                           { Str_Con $1 } 
  | BoolLITERAL                             { Bool_Con $1 }
  | array_literal                           { Array_Con $1  }

array_literal:
  LBRACKET arg_expr_opt RBRACKET            { $2 }

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

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

arg_expr_opt:
  /* nothing */                              { [] }
  | arg_expr_list                            { List.rev $1 }

arg_expr_list:
    expr                                     { [$1] }
  | arg_expr_list COMMA expr                 { $3 :: $1 }

statement:
  | expr NEWLINE                                                                { Expr $1) }
  /*| LBRACE RBRACE NEWLINE                                                       { Brace_Stmt(None) }*/
  | LBRACE NEWLINE statement_list RBRACE NEWLINE                                { Brace_Stmt(List.rev $3) }
  | LOG expr NEWLINE                                                            { Log_Stmt($2) }
  | IF expr COLON LBRACE NEWLINE statement_opt RBRACE NEWLINE elif_statement_list else_statement { If_Stmt(List.rev ($10 :: ($9 @ [ Cond_Exec($2, $6) ]))) }
  | WHILE expr COLON LBRACE NEWLINE statement_opt  RBRACE NEWLINE                   { While_Stmt($2, $6) }
  | FOR ID IN for_in_expr COLON LBRACE NEWLINE statement_opt  RBRACE NEWLINE        { For_In($4, $8) }
  | FOR ID EQ expr TO expr COLON LBRACE NEWLINE  statement_opt  RBRACE NEWLINE      { For_Eq($4, $6, $10)  }
  | CONTINUE NEWLINE                                                            { CONTINUE }
  | BREAK NEWLINE                                                               { BREAK }
  | RETURN expr_opt NEWLINE                                                     { Return($2) }

for_in_expr:
  | ID            {$1}
  | array_literal {$1}

statement_opt:
    /* nothing */ { [] }
  | statement_list { List.rev $1 }

statement_list:
  |  NEWLINE     { [] }
  | statement { Stmt(Some($1)) }
  | statement_list statement { $2 :: $1 }

elif_statement_list:
  | /* nothing */ { [] }
  | ELIF expr COLON LBRACE NEWLINE statement_opt RBRACE NEWLINE elif_statement_list { $9 @ [ Cond_Exec($2, $6) ] }

else_statement:
  | /* nothing */ { }
  | ELSE COLON LBRACE NEWLINE statement_opt RBRACE NEWLINE { Cond_Exec_Fallback($5) }

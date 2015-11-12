%token INDENT NEWLINE
%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA DOT TILDE QUOTATION COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD AND OR AMPERSAND EXCLAMATION
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE ELIF FOR WHILE IN TO CONTINUE BREAK
%token INT DOUBLE BOOL STRING ARRAY
%token FUNC GLOBAL TYPE EVAL
%token FRAME CLIP
%token RIGHTARROW LEFTARROW HAT AT
%token LOG
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

%start expr
%type <int> expr

%%
literal:
    IntLITERAL                              { IntConst $1 }
  | DoubleLITERAL                           { DoubleConst $1 }
  | StringLITERAL                           { StrConst $1 } 
  | BoolLITERAL                             { BoolConst $1 }
  | array_literal                           { ArrayConst $1  }


array_literal:
  LBRACKET arg_expr_opt RBRACKET            { ArrayLit $2 }

primary_expr:
    ID                                               { Id $1 }  
  | primary_expr LBRACKET expr RBRACKET              { ArrayExpr($1, $3) }
  | primary_expr DOT ID                              { DotExpr($1, $3) }

expr:
    primary_expr                             { PrimaryExpr $1 }
  | literal                                  { $1 }
  | expr op expr                             { Call($1, $3, $2) } /* It is not $1 $2 $3 because we want to unify Call with FuncCall in the future*/
  | ID LPAREN arg_expr_opt RPAREN            { Call($1, $3) }
  | LPAREN expr RPAREN                       { ParenExpr $2 }

op:
  | PLUS             { Add     }
  | MINUS            { Sub     }
  | TIMES            { Mult    }
  | DIVIDE           { Div     }
  | EQ               { Equal   }
  | NEQ              { Neq     }
  | LT               { Less    }
  | LEQ              { Leq     }
  | GT               { Greater }
  | GEQ              { Geq     }
  | AND              { And     }
  | OR               { Or      }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

arg_expr_opt:
  /* nothing */                              { [] }
  | arg_expr_list                            { List.rev $1 }

arg_expr_list:
    expr                                     { [$1] }
  | arg_expr_list COMMA expr                 { Args($3 :: $1) }

statement:
   expr NEWLINE                                                                { Expr $1 }
  | primary_expr ASSIGN expr NEWLINE                                            { Assign($1, $3) }
  | LBRACE NEWLINE statement_list RBRACE NEWLINE                                { BraceStmt(List.rev $3) }
  | LOG expr NEWLINE                                                            { LogStmt $2 }
  | IF expr COLON LBRACE NEWLINE statement_opt RBRACE NEWLINE elif_statement_list else_statement { IfStmt(List.rev ($10 :: ($9 @ [ CondExec($2, $6) ]))) }
  | WHILE expr COLON LBRACE NEWLINE statement_opt  RBRACE NEWLINE                   { WhileStmt($2, $6) }
  | FOR ID IN for_in_expr COLON LBRACE NEWLINE statement_opt  RBRACE NEWLINE        { ForIn($2, $4, $8) }
  | FOR ID EQ expr TO expr COLON LBRACE NEWLINE  statement_opt  RBRACE NEWLINE      { ForEq($2, $4, $6, $10)  }

  | CONTINUE NEWLINE                                                            { CONTINUE }
  | BREAK NEWLINE                                                               { BREAK }
  | RETURN expr_opt NEWLINE                                                     { Return($2) }

for_in_expr:
   ID            {$1}
  | array_literal {$1}

statement_opt:
    /* nothing */ { [] }
  | statement_list { List.rev $1 }

statement_list:
    NEWLINE     { [] }
  | statement { Stmt(Some($1)) }
  | statement_list statement { $2 :: $1 }

elif_statement_list:
   /* nothing */ { [] }
  | ELIF expr COLON LBRACE NEWLINE statement_opt RBRACE NEWLINE elif_statement_list { ElifStmt($9 @ [ CondExec($2, $6) ]) }

else_statement:
   ELSE COLON LBRACE NEWLINE statement_opt RBRACE NEWLINE { ElseStmt(CondExecFallback($5))}

type_def:
  TYPE type_name COLON NEWLINE LBRACE type_element_list RBRACE    {TypeDef($2, $6)}

type_name:
  ID    {Id $1}

type_element_list:
  /*  nothing */  { [] }
  | value_decl NEWLINE type_element_list  {TypeEleList($1, $3)}
  | func_decl NEWLINE type_element_list {TypeEleList($1, $3)}
  | type_def NEWLINE type_element_list {TypeEleList($1, $3)}

value_decl:
  ID COLON type_name  {ValueDecl($1, $3)}

func_decl:
    FUNC func_name LBRACE RBRACE COLON NEWLINE LBRACKET statement_list RBRACKET       {FuncDecl($2, $8)}
  | FUNC func_name LBRACE func_arg_list RBRACE COLON NEWLINE LBRACKET statement_list RBRACKET       {FuncDecl($2, $4, $9)}

func_name:
  ID        {Id $1}

func_arg_list:
   ID COLON type_name    { FuncArgList($1, $3) }
  | func_arg_list COMMA ID COLON type_name    { $1 :: FuncArgList($3, $5)}


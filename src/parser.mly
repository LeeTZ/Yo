%{ 
    open Ast
%}

%token SEMI LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA DOT TILDE QUOTATION COLON
%token PLUS MINUS TIMES DIVIDE ASSIGN MOD AND OR AMPERSAND EXCLAMATION
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE ELIF FOR WHILE IN TO DOWNTO CONTINUE BREAK
/*%token INT DOUBLE BOOL STRING ARRAY FRAME CLIP*/
%token FUNCTION TYPE /*EVAL*/
%token RIGHTARROW /*LEFTARROW*/ HAT AT
%token TRUE FALSE
%token <int> IntLITERAL 
%token <float> DoubleLITERAL
%token <string> StringLITERAL
%token <bool> BoolLITERAL
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%left RPAREN RBRACKET
%right LPAREN LBRACKET
%left EQ NEQ
%left HAT AT
%left AMPERSAND
%left OR
%left AND
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%left DOT

%start global
%type <Ast.program> global

%%
literal:
    | IntLITERAL                              { IntConst $1 }
    | DoubleLITERAL                           { DoubleConst $1 }
    | StringLITERAL                           { StrConst $1 } 
    | BoolLITERAL                             { BoolConst $1 }
    | array_literal                           { $1  }

arg_expr_opt:
    /* nothing */                              { [] }
    | arg_expr_list                            { List.rev $1 }

arg_expr_list:
    | expr                                  { [$1] }
    | arg_expr_list COMMA expr              { $3 :: $1 }

array_literal:
    LBRACKET arg_expr_list RBRACKET         { ArrayConst (List.rev $2) }

type_base:
    | ID                                      { SimpleType $1 }
    | type_base DOT ID                      { NestedType($1, $3) }

type_name:
    type_base                               { $1 }
    | type_name LBRACKET RBRACKET           { ArrayType $1 }

expr:
    | ID                                    { Var $1}
    | literal                               {$1}
    | LPAREN expr RPAREN                    {$2}
    | expr PLUS   expr                      { Binop($1, Add,   $3) }
    | expr MINUS  expr                      { Binop($1, Sub,   $3) }
    | expr TIMES  expr                      { Binop($1, Mult,  $3) }
    | expr DIVIDE expr                      { Binop($1, Div,   $3) }
    | expr MOD      expr                    { Binop($1, Mod,   $3) }
    | expr EQ     expr                      { Binop($1, Eq,    $3) }
    | expr NEQ    expr                      { Binop($1, Neq,   $3) }
    | expr LT     expr                      { Binop($1, Less,  $3) }
    | expr LEQ    expr                      { Binop($1, Leq,   $3) }
    | expr GT     expr                      { Binop($1, Gt,    $3) }
    | expr GEQ    expr                      { Binop($1, Geq,   $3) }
    | expr AND      expr                    { Binop($1, And,   $3) }
    | expr OR           expr                { Binop($1, Or,   $3) }
    | dot_expr                           { $1 }
    | expr LBRACKET expr RBRACKET           { ArrayIndex($1, $3) }
    | expr LBRACKET expr COLON expr RBRACKET { ArrayRange($1, $3, $5) }
    | expr HAT expr AT expr                 { ClipConcat($1, $3, $5) }
    | expr DOT ID LPAREN arg_expr_opt RPAREN { Call(Some($1), $3, $5) }
    | ID LPAREN arg_expr_opt RPAREN         { Call(None, $1, $3) }
    | array_constructor LPAREN arg_expr_opt RPAREN { BuildArray($1, $3) }

dot_expr:
    | expr DOT ID  { DotExpr($1, $3) }

array_constructor:
    | expr LBRACKET RBRACKET { SimpleArrayConstructor $1 }
    | array_constructor LBRACKET RBRACKET { CompositeArrayConstructor $1}

expr_opt:
    /* nothing */ { None }
    | expr          { Some($1) }

statement:
    | expr SEMI                                                            { Assign(None, $1) }
    | expr ASSIGN expr SEMI                                                { Assign(Some($1), $3) }
    | dot_expr AT expr ASSIGN expr SEMI                                        { SetAttribute($1, $3, $5) }
    | IF expr COLON LBRACE statement_opt RBRACE elif_statement_list else_statement { IfStmt(List.rev ($8 @ $7 @ [ CondExec(Some($2), $5) ])) }
    | WHILE expr COLON LBRACE statement_opt  RBRACE                        { WhileStmt($2, $5) }
    | FOR ID IN for_in_expr COLON LBRACE  statement_opt RBRACE             { ForIn($2, $4, $7) }
    | FOR ID ASSIGN expr TO expr COLON LBRACE statement_opt RBRACE         { ForRange($2, $4, $6, $9, Inc) }
    | FOR ID ASSIGN expr DOWNTO expr COLON LBRACE statement_opt RBRACE     { ForRange($2, $4, $6, $9, Dec) }
    | CONTINUE SEMI                                                        { Continue }
    | BREAK SEMI                                                           { Break }
    | RETURN expr_opt SEMI                                                 { Return $2 }

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
    ID COLON type_name  { VarDecl($1, $3) }

mem_var_decl:
    var_decl SEMI  { MemVarDecl($1) }

func_decl:
    FUNCTION ID LPAREN func_arg_opt RPAREN RIGHTARROW type_name COLON LBRACE statement_opt RBRACE      {FuncDecl($2, $4, $7, $10)}

mem_func_decl:
    func_decl { MemFuncDecl($1) }

global_func_decl:
    func_decl { GlobalFunc($1) }

func_arg_opt:
    /* nothing */        { [] }
    | func_arg_list         { List.rev $1 }

func_arg_list:
    | var_decl                                              { [$1] }
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
    | mem_var_decl  type_element_list { $1 :: $2 }
    | mem_func_decl type_element_list { $1 :: $2 }
    | mem_type_decl type_element_list { $1 :: $2 }

global_ele:
    | global_func_decl  { $1 }
    | global_type_decl  { $1 }
    | global_statement  { $1 }

global_ele_list:
    | global_ele                        { [$1] }
    | global_ele_list global_ele    { $2 :: $1 }

global:
    global_ele_list { List.rev $1 }
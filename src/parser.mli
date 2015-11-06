type token =
  | INDENT
  | NEWLINE
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COMMA
  | DOT
  | TILDE
  | QUOTATION
  | COLON
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | MOD
  | AND
  | OR
  | AMPERSAND
  | EXCLAMATION
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RETURN
  | IF
  | ELSE
  | ELIF
  | FOR
  | WHILE
  | IN
  | TO
  | CONTINUE
  | BREAK
  | INT
  | DOUBLE
  | BOOL
  | STRING
  | ARRAY
  | LAMBDA
  | FUNCTION
  | GLOBAL
  | TYPE
  | EVAL
  | FRAME
  | CLIP
  | RIGHTARROW
  | LEFTARROW
  | CASCADE
  | LOG
  | IntLITERAL of (int)
  | DoubleLITERAL of (float)
  | StringLITERAL of (string)
  | BoolLITERAL of (bool)
  | ID of (string)
  | EOF

val statement_opt :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int

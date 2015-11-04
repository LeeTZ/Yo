{ open Parser }

let Integer_cons = '-'? ['0'-'9']+
let Double_cons = '-'? ['0'-'9']+ '.' ['0'-'9']+
let String_cons = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  [' ' '\r'] { token lexbuf } (* Whitespace *)
| '\'      { continue lexbuf }
| "#{"     { comment lexbuf }           (* Comments *)
| '#'      { oneLineComment lexbuf}
| '\t'     { INDENT }
| '\n'     { NEWLINE }

| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '.'      { DOT }
| '~'      { TILDE }
| '"'      { QUOTATION }
| ':'      { COLON }

| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| '>'      { GT }
| ">="     { GEQ }
| '%'      { MOD }
| "&&"     { AND }
| '&'      { AMPERSAND }
| "||"     { OR }
| '!'	   { EXCLAMATION }

| "Int"    { INT }
| "Double" { DOUBLE }
| "Bool"   { BOOL }
| "String" { STRING }
| "Array"  { ARRAY }

| "Frame"  { FRAME }
| "Clip"   { CLIP }

| "->"     { RIGHTARROW }
| "<-"     { LEFTARROW }
| "^@"     { CASCADE }

| "if"     { IF }
| "else"   { ELSE }
| "in"     { IN }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "continue" { CONTINUE }
| "break"  { BREAK }

| "lambda" { LAMBDA }
| "func"   { FUNCTION }
| "global" { GLOBAL }
| "type"   { TYPE }

| "log"    { LOG }
| "eval"   { EVAL }

| Integer_cons as lxm { LITERALINT(int_of_string lxm) }
| Double_cons as lxm { LITERALDOUBLE(float_of_string lxm) }
| String_cons as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "#}" { token lexbuf }
| _    { comment lexbuf }

and oneLineComment = parse
  '\n' { token lexbuf }
| _    { oneLineComment lexbuf }

and continue = parse
  [' ' '\t' '\r' '\n'] {continue lexbuf}
| [^ ' ' '\t' '\r' '\n'] {token lexbuf}
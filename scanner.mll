{ open Parser }

rule token = parse
  [' ' '\r' ('\' [' ' '\t']* '\n' [' ' '\t']*)] { token lexbuf } (* Whitespace *)
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

| ">-"     { RIGHTARROW }
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

(* in the future, those Built-in function may be eliminated from the scanner *)
| "log"    { LOG }
| "readFrame" { READFRAME }
| "writeFrame" { WRITEFRAME }


| '-'? ['0'-'9']+ as lxm { LITERALINT(int_of_string lxm) }
| '-'? (['0'-'9']* '.' ['0'-'9']+ ('e' ('+'|'-')? ['0'-'9']+)? | ['0'-'9']+ '.'? ('e' ('+'|'-')? ['0'-'9']+) | ['0'-'9']+ '.') 
	as lxm { LITERALDOUBLE(float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "#}" { token lexbuf }
| _    { comment lexbuf }

and oneLineComment = parse
  '\n' { token lexbuf }
| _    { oneLineComment lexbuf }
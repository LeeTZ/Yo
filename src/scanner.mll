{ open Parser }

let Integer_cons = '-'? ['0'-'9']+
let Double_cons = '-'? ['0'-'9']+ '.' ['0'-'9']+
let Id_cons = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let String_cons = [^ '"' ]* (* Is that correct? *)

rule token = parse
  [' ' '\r'] { token lexbuf } (* Whitespace *)
(*| '\'      { continue lexbuf }*)
| "#("     { comment lexbuf }           (* Comments *)
| '#'      { oneLineComment lexbuf}
| '\t'     { INDENT }
| '\n'     { NEWLINE }

| '('      { LPAREN }
| ')'      { RPAREN }
| '['	   { LBRACKET }
| ']'      { RBRACKET }
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

(*| "->"     { RIGHTARROW }
| "<-"     { LEFTARROW }*)
| "^"      { HAT }
| "@"		{ AT }

| "if"     { IF }
| "else"   { ELSE }
| "elif"   { ELIF }
| "in"     { IN }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "continue" { CONTINUE }
| "break"  { BREAK }

(*| "lambda" { LAMBDA }*)
| "func"   { FUNCTION }
(* | "global" { GLOBAL } *)
| "type"   { TYPE }

| "log"    { LOG }
| "eval"   { EVAL }
| "true"	as lxm	{ BoolLITERAL(bool_of_string lxm) }
| "false"	as lxm	{ BoolLITERAL(bool_of_string lxm) }

| Integer_cons as lxm { IntLITERAL(int_of_string lxm) }
| Double_cons as lxm { DoubleLITERAL(float_of_string lxm) }
| '"' String_cons '"' as lxm { StringLITERAL(lxm) }
| Id_cons as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "#)" { token lexbuf }
| _    { comment lexbuf }

and oneLineComment = parse
  '\n' { token lexbuf }
| _    { oneLineComment lexbuf }

(*and continue = parse
  [' ' '\t' '\r' '\n'] {continue lexbuf}
| [^ ' ' '\t' '\r' '\n'] {token lexbuf}*)
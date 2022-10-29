{
open Parser
exception EOF
}
let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? '+'? digit+
let float = '-'? '+'? digit+ '.' digit+

rule read =
	parse
	| white { read lexbuf }
	| '\n' { EOL }
	| "(" { RPAR }
	| ")" { LPAR }
	| "/" { DIV }
	| "%" { MOD }
	| "*." { MULF }
	| "+." { PLUSF }
	| "-." { SUBF }
	| "*" { MUL }
	| "+" { PLUS }
	| "-" { SUB }
	| float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
	| int { INT (int_of_string (Lexing.lexeme lexbuf)) }
	| eof { EOF }

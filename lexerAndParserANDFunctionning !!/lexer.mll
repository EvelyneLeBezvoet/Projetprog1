{
open Parser
exception EOF
}
let white = [' ' '\t']+
let digit = ['0'-'9']
let int = digit+
let float1 = digit+? '.' digit+
let float2 = digit+ '.' digit+?

rule read =
        parse
        | white { read lexbuf }
        | '\n' { EOL }
        | "(" { LPAR }
        | ")" { RPAR }
        | "/" { DIV }
        | "%" { MOD }
        | "*." { MULF }
        | "+." { PLUSF }
        | "-." { SUBF }
        | "*" { MUL }
        | "+" { PLUS }
        | "-" { SUB }
        | float1 { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
        | float2 { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
        | digit+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
        | eof { EOF }

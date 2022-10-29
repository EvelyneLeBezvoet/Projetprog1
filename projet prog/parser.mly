%{
open Ast
%}

%token <int> INT
%token MUL
%token DIV
%token MOD
%token PLUS
%token SUB

%token <float> FLOAT
%token  MULF
%token  PLUSF
%token  SUBF

%token LPAR
%token RPAR
%token EOL
%token EOF

%start prog
%type <Ast.exp> prog

%left PLUSF PLUS
%left SUBF SUB
%left MULF MUL
%left MOD
%left DIV
%left MUL



%%

prog:
	|exprint {I $1}
	|exprfloat {F $1}

exprint:
	|INT { Int $1 }
	| exprint MUL exprint { Binop (Mul, $1, $3) }
	| exprint PLUS exprint EOF { Binop (Add, $1, $3) }
	| exprint SUB exprint { Binop (Sub, $1, $3) }
	| exprint DIV exprint { Binop (Div, $1, $3) }
	| exprint MOD exprint { Binop (Mod, $1, $3) }
	| PLUS exprint { Unop ( Plus, $2)}
	| SUB exprint { Unop ( Minus, $2)}
	| LPAR exprint RPAR {$2}
	

exprfloat:
	|FLOAT { Float $1 }
	| exprfloat MULF exprfloat { Binopf (Mulf, $1, $3) }
	| exprfloat PLUSF exprfloat { Binopf (Addf, $1, $3) }
	| exprfloat SUBF exprfloat { Binopf (Subf, $1, $3) }
	| PLUS exprfloat { Unopf ( Plusf, $2) }
	| SUB exprfloat { Unopf ( Minusf, $2) }
	| LPAR exprfloat RPAR { $2 }
	

%%


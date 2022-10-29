type bopint =
	| Add
	| Sub
	| Mul
	| Div
	| Mod

type uopint =
	| Plus
	| Minus

type exprint =
	| Int of int
	| Binop of bopint * exprint * exprint
	| Unop of uopint * exprint



type bopfloat =
	| Addf
	| Subf
	| Mulf

type uopfloat =
	| Plusf
	| Minusf

type exprfloat =
	| Float of float
	| Binopf of bopfloat * exprfloat * exprfloat
	| Unopf of uopfloat * exprfloat

type exp =
	| I of exprint
	| F of exprfloat

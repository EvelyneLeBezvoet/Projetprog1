(* On sépare les opérateurs spécifiques aux entiers et aux flottants
pour que le parser détecte les erreurs de typage dans les formules*)


type bopint =
        | Add
        | Sub
        | Mul
        | Div
        | Mod

(* pour représenter +5 ou -(3+4) par exemple *)
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

(*on réunit les deux types d'écritures sous un même type exp*)
type exp =
        | I of exprint
        | F of exprfloat

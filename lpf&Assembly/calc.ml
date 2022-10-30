(* File calc.ml *)
open Ast;;

let print_op = function
        | Add -> print_string "Add"
        | Sub -> print_string "Sub"
        | Mul -> print_string "Mul"
        | Div -> print_string "Div"
        | Mod -> print_string "Mod";;

let print_opf = function
        | Addf -> print_string "Addf"
        | Subf -> print_string "Subf"
        | Mulf -> print_string "Mulf";;

let print_uop = function
        | Plus -> print_string "Plus"
        | Minus -> print_string "Minus";;

let print_uopf = function
        | Plusf -> print_string "Plusf"
        | Minusf -> print_string "Minusf";;

let rec print_exprint e = match e with
        | Int a -> print_int a
        | Binop (op,a,b) -> print_op op ; print_string "(\n\t"; print_exprint a ; print_string ", " ; print_exprint b; print_string ")"
        | Unop (uop,a) -> print_uop uop ; print_string "(\n\t"; print_exprint a ; print_string ")";;

let rec print_exprfloat e = match e with
        | Float a -> print_float a
        | Binopf (opf,a,b) -> print_opf opf ; print_string "(\n\t"; print_exprfloat a ; print_string ", " ; print_exprfloat b ; print_string ")"
        | Unopf (uopf,a) -> print_uopf uopf ; print_string "(\n\t"; print_exprfloat a ; print_string ")";;

let print_exp expr = match expr with
        | I e -> print_exprint e
        | F e -> print_exprfloat e;;

let nomfichier = Sys.argv.(1)
let nomExecutable = String.sub nomfichier 0 ((String.length nomfichier) - 4)   (*nom de l'executable*)
let result =
let text = Arg.read_arg Sys.argv.(1) in
  let lexbuf = Lexing.from_string (text.(0)) in
  Parser.prog Lexer.read lexbuf;;

(* in
  (print_exp result; print_newline(); flush stdout) *)

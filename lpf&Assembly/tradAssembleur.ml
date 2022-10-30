open X86_64
open Ast
open Calc


(*permet de coller une liste d'éléments de asm et donne un élément de asm*)
let rec cc = function
  | [] -> nop
  | [a] -> a
  | hd::tl -> (++) hd (cc tl);;

(*écrit la portion de texte nécessaire au programme:
on utilise une pile. On dépile les valeurs nécessaires à une opération
et on empile la valeur obtenue à la fin du calcul.*)
let rec writeAsInt = function
  | Int a -> cc [movq (imm a) (reg rdi); pushq (reg rdi)]
  | Binop (op,exp1,exp2) -> cc [writeAsInt exp1; writeAsInt exp2;
                                popq rdi; popq rsi;
                                (match op with
                                 | Add -> addq (reg rsi) (reg rdi)
                                 | Sub -> subq (reg rsi) (reg rdi)
                                 | Mul -> imulq (reg rsi) (reg rdi)
                                 | op2 -> cc [movq (reg rsi) (reg rax);
                                              movq (imm 0) (reg rdx); idivq (reg rdi);
                                              (match op2 with
                                               | Div -> movq (reg rax) (reg rdi)
                                               | Mod -> movq (reg rdx) (reg rdi)
                                               | _ -> nop
                                              )]
                                );
                                pushq (reg rdi)]
  | Unop (Plus, exp1) -> writeAsInt exp1
  | Unop (Minus, exp1) -> cc [writeAsInt exp1; popq rdi; movq (imm (-1)) (reg rax);
                              imulq (reg rdi) (reg rax); pushq (reg rax)]


  (* tests :
let x = writeAsInt result;;
let y = writeAsInt (Binop(Div,Int 6,Int 3));;
let ybis = writeAsInt (Binop(Mod,Int 6, Int 2));;
let z = writeAsInt (Binop(Mul,Int 5, Int 4));;
let w = writeAsInt (Binop(Sub,Int 4, Int 2));; *)

(*On sépare l'écriture des expressions entières et flottantes*)
let writeAs = function
  | I exp -> writeAsInt exp
  | F exp -> nop (*writeAsFloat exp*)

(*on crée le programme*)
let makeprog exp =
  {text = (cc [inline "\t.globl main\nmain:\n";writeAs exp;
               inline "\tpopq %rdi\n\tcall print_int\n\tret\n

print_int:
        movq %rdi, %rsi
        movq $message, %rdi
        call printf
        ret\n\n"]);
   data = (inline "message:\n\t.string \"%d \"");
  }


let masterprog = makeprog result;;
print_in_file (nomExecutable^".s") masterprog;;  (*on écrit masterprog dans le fichier <nomExecutable>.s*)

open Ast
open X86_64
open Calc

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
let x = ref 0;;
let prog = ref {text = nop; data = nop};;

let writeAsFloatInter exp =
let rec aux = function
  | Float a -> (prog := {text = cc [(!prog).text;inline ("\tmovsd  .LC"^(string_of_int !x)^"(%rip), %xmm0
                    \n\tmovsd %xmm0, -8(%rsp)\n\tsubq $8, %rsp\n")];
                         data = cc [(!prog).data;inline (".LC"^(string_of_int !x)^":\n\t.double "^(string_of_float a)^"\n")]};
                incr x)
  | Binopf(Addf, e1, e2) -> (aux e1; aux e2;
                             (prog := {text = (cc [(!prog).text;
                                                   inline ("\tmovsd   (%rsp), %xmm0\n\tmovsd   8(%rsp), %xmm1\n\t
                               addq    $16, %rsp\n\taddsd %xmm0, %xmm1\n\tmovsd %xmm1, -8(%rsp)\n\tsubq $8, %rsp")]);
                                       data = (!prog).data}));
                                       in aux exp;
(!prog);;

let rec writeAsFloat prog = prog.text;;




(*let blup = Binopf(Addf, Float 6.5, Float 3.);;
let rep = writeAsFloatInter blup;;
  *)




(*let result = F (Binopf(Addf,Float 6.34,Float 3.2))*)
  (* tests :
let x = writeAsInt result;;
let y = writeAsInt (Binop(Div,Int 6,Int 3));;
let ybis = writeAsInt (Binop(Mod,Int 6, Int 2));;
let z = writeAsInt (Binop(Mul,Int 5, Int 4));;
let w = writeAsInt (Binop(Sub,Int 4, Int 2));; *)

(*On sépare l'écriture des expressions entières et flottantes*)
let writeAs = function
  | I exp -> writeAsInt exp
  | F exp -> writeAsFloat (writeAsFloatInter exp)
 
 let makeprog = function
  | I exp ->
      {text = (cc [inline "\t.globl main\nmain:\n";writeAs (I exp);
                   inline "\tpopq %rdi\n\tcall print_int\n\tret\n

                                                                                print_int:
                         movq %rdi, %rsi
                         movq $message, %rdi
                         call printf
                         ret\n\n"]);
       data = (inline "message:\n\t.string \"%d \"");
      }
  | F exp ->
      {text = (cc [inline "\t.globl main\nmain:\n";writeAs (F exp);
                   inline "\n\tmovsd   (%rsp), %xmm0\n\taddq    $8, %rsp\n\tmovq    %xmm0, %rdi
                          \n\tcall print_float\n\tret
       \nprint_float:\n\tmovq    %rsp, %rbp\n\tmovq    %rdi, %xmm0\n\t movl    $convfloat, %edi\n\tmovl    $1, %eax\n\tcall    printf\n\tret\n"]);
       data = (cc [inline "message:\n\tconvfloat:\n\t.string \"%g\"\n"; (writeAsFloatInter exp).data])}


let masterprog = makeprog result;;
print_in_file (nomExecutable^".s") masterprog;;

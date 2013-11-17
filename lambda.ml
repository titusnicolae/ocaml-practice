module M = Map.Make(String)
module S = Set.Make(String)

type var = Variable of string * int ref

type expr=
    | Var of var            (* x *)
    | Lambda of var * expr     (* λx.e *)
    | Eval of expr * expr   (* e1 e2 *)

let print_variable = function
    |  Variable(s,n) -> if !n==0 then s
                        else s^(string_of_int !n)
    
let rec print = function 
    | Var(a)-> print_variable a 
    | Lambda (a,expr) -> "λ"^(print_variable a)^"."^(print expr)
    | Eval (expr1,expr2) -> "("^(print expr1)^" "^(print expr2)^")"  


let def = fun x -> Variable(x,ref 0)
let defv = fun x -> Var(Variable(x,ref 0))

let s = Lambda(def("x"),defv("x"))

let _ = print_string (print s)
let _ = print_string "\n"










(*
let rec fix = function
    | (Variable(y,ny),Var(Variable(x,nx)) ->
        if (compare y x) == 0 && !ny == !nx 
        then incr ny   
*)

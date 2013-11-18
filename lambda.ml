module M = Map.Make(String)
module S = Set.Make(String)

type var = Variable of string * int ref

type expr=
    | Var of var            (* x *)
    | Lambda of var * expr  (* λx.e *)
    | Eval of expr * expr   (* e1 e2 *)

let print_variable = function
    |  Variable(s,n) -> if !n==0 then s
                        else s^(string_of_int !n)
    
let rec print = function 
    | Var(a)-> print_variable a 
    | Lambda (a,expr) -> "λ"^(print_variable a)^"."^(print expr)
    | Eval (expr1,expr2) -> "("^(print expr1)^" "^(print expr2)^")"  

let rec eval = function
		| Eval(Var(x), _) -> ""    	
	  | Eval( Eval(e1, e2), e3) -> ""

		| Eval( Lambda (Variable(x,n),e), Var(y) ) -> ""   
		| Eval( Lambda (Variable(x,n),e1), Lambda(Variable(y,m), e2 )) -> ""   
		| Eval( Lambda (Variable(x,n),e1), Eval(e2,e3)) -> ""   
	
	   

let v = fun x -> Variable(x,ref 0)
let _v = fun x -> Var(Variable(x,ref 0))

let vn = fun x y -> Variable(x, ref y)
let _vn = fun x y -> Var(Variable(x, ref y))

let l = fun x y -> (Lambda(x,y))  
let a = fun x y -> (Eval(x,y))

let s = (a (a (l (v "x") (l (v "y") (_v "x"))) (_v "y")) (_v "z")) 

let _ = print_string (print s)
let _ = print_string "\n"




open Formule

let alpha = Var "alpha"

let x = Var "x"

let abs x = Function ("abs", [x])
let leq x alpha = Atome ("<=", ([x; alpha]))
let geq x alpha= Atome (">=",[abs x; alpha])

let f1 = PourTout ("x", Imp(leq x alpha, geq x alpha)) 

let f2 = string_of_formule f1
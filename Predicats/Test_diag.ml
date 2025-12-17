open Formule_Syllogisme
open Proposition.Formule
(*open DiagVenn*)

let a = Atome "a"
let b = Atome "b"
let c = Atome "c"
let d = Atome "d"
let p1 = PourTout (Imp (Ou (a, b), c))
let p2 = PourTout (Imp (c, Ou (a, b)))
let p3 = IlExiste a
let p4 = IlExiste (Imp (a, Non b))

let p5 =
  let xor (a, b) = Ou (Et (a, Non b), Et (Non a, b)) in
  PourTout (Imp (xor (a, b), c))

let p6 = PourTout (Imp (Non c, a))
let p7 = IlExiste (Et (Et (a, c), Non b))
let c1 = IlExiste b
let c2 = IlExiste c
let c3 = IlExiste d

(** test premisses conclusion : teste si chacun des diagrammes de la combinaison
    de la liste prémisses est compatible avec au moins un des diagrammes de
    conclusion, tout en traçant les calculs réalisés et les diagrammes calculés,
    et en affichant tous les contre-exemples le cas échéant. *)
let test (_ : formule_syllogisme list) (_ : formule_syllogisme) : unit =
  failwith "test : à faire"

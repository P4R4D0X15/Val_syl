type bot = |
type ('a, 'b) et = 'a * 'b
type ('a, 'b) ou = ('a, 'b) Either.t
type ('a, 'b) imp = 'a -> 'b
type 'a non = 'a -> bot

(* Exercice 1 : règles introduction *)
let imp_intro : ('a -> 'b) -> 'a -> 'b = Fun.id
let et_intro : 'a * 'b -> 'a * 'b = Fun.id
let ou_intro_g : 'a -> ('a, 'b) Either.t = fun a ->  (Either.Left a)
let ou_intro_d : 'b -> ('a, 'b) Either.t = fun b -> (Either.Right b)

(* Exercice 2 : règles d'élimination *)
let bot_elim (bot : bot) : 'a = match bot with _ -> .

let ou_elim : ('a, 'b) Either.t * ('a -> 'c) * ('b -> 'c) -> 'c =
 fun (e, ac, bc) -> match e with
                        | Either.Left a -> ac a 
                        | Either.Right b -> bc b

let imp_elim : ('a -> 'b) * 'a -> 'b = fun (ab, a) -> ab a
let et_elim_g : 'a * 'b -> 'a = fun (a, _) -> a
let et_elim_d : 'a * 'b -> 'b = fun (_, b) -> b

(* Exercice 3 : déduction naturelle *)

(* Exemple : principe de non-contradiction *)
let non_contra : 'a * ('a -> bot) -> bot = fun (a, non_a) -> non_a a
let non_contra2 : ('a, 'a non) et non = fun (a, non_a) -> non_a a

let non_contra' : 'a * ('a -> bot) -> bot =
 fun a_et_non_a -> imp_elim (et_elim_d a_et_non_a, et_elim_g a_et_non_a)

let f1 : 'a -> 'a = fun a -> a

let f2 : ('a non, 'b) ou * 'a -> 'b = fun (d, a) -> 
        match d with
            | Either.Left na -> bot_elim (na a)
            | Either.Right b -> b

let f3 : ('a, 'b) ou -> ('b, 'a) ou = fun d -> 
    match d with
        | Either.Left a -> Either.Right a
        | Either.Right b -> Either.Left b

let f4 : 'a * 'b -> 'b * 'a = fun (a, b) -> (b, a)

let f5 : 'a * 'b -> ('a, 'b) ou = fun (a, _) -> Either.Left a

let f6 : 'a -> 'a non non = fun a nna -> nna a

let f7 : ('a non, 'b) ou -> 'a -> 'b = fun d a -> 
    match d with
        | Either.Left na -> bot_elim (na a)
        | Either.Right b -> b

let f8 : ('a * 'b -> 'c) -> 'a -> 'b -> 'c = fun f a b -> f (a, b)

let f9 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c = fun f (a, b) -> f a b

let f10 : ('a non, 'b non) ou -> ('a * 'b) non = fun d (a,b) -> 
    match d with
        | Either.Left na -> na a
        | Either.Right _ -> b

let f11 : 'a non * 'b non -> ('a, 'b) ou non = fun (na, nb) d -> 
    match d with
        | Either.Left a -> na a
        | Either.Right b -> nb b
        
let f12 : 'a -> 'b -> 'a = fun _ _ -> failwith "afer"

let f13 : ('a -> 'b -> 'c) -> ('a -> 'b) -> 'a -> 'c = fun abc ab a -> ab (abc a)

let f14 : ('a -> 'b) -> 'b non -> 'a non = fun ab nb a -> nb (ab a)

let f15 : ('a -> 'b non) -> 'b -> 'a non = fun anb b a -> (anb a) b

let f16 : 'a -> 'a non -> 'b = fun a na -> bot_elim (na a)

let f17 : 'a * ('b, 'c) ou -> ('a * 'b, 'a * 'c) ou =
 fun (a, dbc) -> match dbc with 
    | Either.Left b -> Either.Left (a, b)
    | Either.Right c -> Either.Right (a, c)

let f18 : ('a * 'b, 'a * 'c) ou -> 'a * ('b, 'c) ou =
 fun dabac ->
    (   match dabac with
            | Either.Left (a, _) | Either.Right (a, _) -> a
    ),(
        match dabac with
            | Either.Left (_, b) -> Either.Left b
            | Either.Right (_, c) -> Either.Right c
    )

let f19 : ('a, 'b * 'c) ou -> ('a, 'b) ou * ('a, 'c) ou =
 fun dabc -> 
    (   match dabc with
            | Either.Left a -> Either.Left a
            | Either.Right (b, _) -> Either.Right b
    ),(
        match dabc with
            | Either.Left a -> Either.Left a
            | Either.Right (_, c) -> Either.Right c
    )

let f20 : ('a, 'b) ou * ('a, 'c) ou -> ('a, 'b * 'c) ou =
 fun (dab, dac) -> match dab, dac with
                    | Either.Left a , _ | _, Either.Left a -> Either.Left a
                    | Either.Right b , Either.Right c -> Either.Right (b, c)

let f21 : ('a -> 'b) -> ('a * 'b non) non = fun ab (a, nb) -> nb(ab a) 

let f22 : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = fun ab bc a -> bc (ab a)

let f23 : 'a non non non -> 'a non = fun nnna a -> let nna na = na a in nnna nna 

let f24 : ('a, 'a non) ou non non = fun _ -> failwith "à faire"

let f25 : ('a, 'b) ou non -> 'a non * 'b non = fun _ -> failwith "afer";;


(* Exercice 4 : déduction naturelle et tiers exclu *)
type 'a tiers_exclu = ('a, 'a non) ou

let g1 : 'a tiers_exclu -> ('a non -> 'b non) -> 'b -> 'a =
 fun _ _ _ -> failwith "à faire"

let g2 : 'a tiers_exclu -> ('a non -> 'b) -> 'b non -> 'a =
 fun _ _ _ -> failwith "à faire"

let g3 : 'a tiers_exclu -> ('a -> 'b, 'b -> 'a) ou = fun _ -> failwith "à faire"

let g4 :
    'a tiers_exclu -> 'b tiers_exclu -> ('a, 'b) et non -> ('a non, 'b non) ou =
 fun ta tb _ -> match ta with 
                    | Either.Left _ -> (match tb with
                                        | Either.Left _ -> (*nab (a, b)*) failwith "afer"
                                        | Either.Right nb -> Either.Right nb)
                    | Either.Right na -> Either.Left na

let g5 : 'a tiers_exclu -> 'a non non -> 'a = fun ta nna -> 
    match ta with
    | Either.Left a -> a
    | Either.Right na -> bot_elim (nna na)

let g6 : 'a tiers_exclu -> ('a -> 'b) -> ('a non, 'b) ou =
 fun te ab -> match te with
                | Either.Left a -> Either.Left (ab a)
                | Either.Right na -> Either.Left na

let g7 : 'a tiers_exclu -> ('a -> 'b) tiers_exclu -> (('a -> 'b) -> 'a) -> 'a =
 fun tea _ aba -> 
    match tea with
    | Either.Left a -> a
    | Either.Right na -> 
        let g a = bot_elim (na a)
        in aba g

let g8 : 'b tiers_exclu -> ('a * 'b non) non -> 'a -> 'b =
 fun _ _ _ -> failwith "à faire"

let g9 : 'a tiers_exclu -> ('a non -> 'b non) -> ('a non -> 'b) -> 'a =
 fun ta nanb nab -> 
    match ta with
    | Either.Left a -> a
    | Either.Right na -> bot_elim ((nanb na)(nab na))

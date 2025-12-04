(** Le module Formule contient les types et définitions de base permettant la
    manipulation des formules de la logique propositionnelle. *)

(** Type des formules de la logique propositionnelle, avec des string comme
    atomes. *)
type formule =
  | Bot
  | Top
  | Atome of string
  | Imp of (formule * formule)
  | Ou of (formule * formule)
  | Et of (formule * formule)
  | Non of formule
  | Equiv of (formule * formule)

(* Fonction de construction d'atome. *)
(* let atome x = Atome x *)

(* ----------------- Exercice 1 : Hauteur ----------------- *)

(** Calcule la hauteur de l'arbre syntaxique d'une formule. *)
let rec hauteur (f : formule) : int =
  match f with
  | Bot | Top | Atome _ -> 1
  | Imp (f1, f2) | Ou (f1, f2) | Et (f1, f2) | Equiv (f1, f2) ->
      1 + max (hauteur f1) (hauteur f2)
  | Non f1 -> 1 + hauteur f1

(* ----------------- Exercice 2 : Représentation en chaîne de caractères ----------------- *)

(** Conversion d'une formule en chaîne de caractères. *)
let string_of_formule (f : formule) : string =
  let rec aux f =
    match f with
    | Imp (f1, f2) -> "(" ^ aux f1 ^ " -> " ^ aux f2 ^ ")"
    | Ou (f1, f2) -> "(" ^ aux f1 ^ " v " ^ aux f2 ^ ")"
    | Et (f1, f2) -> "(" ^ aux f1 ^ " ∧ " ^ aux f2 ^ ")"
    | Equiv (f1, f2) -> "(" ^ aux f1 ^ " <=> " ^ aux f2 ^ ")"
    | Bot -> "⊥"
    | Top -> "T"
    | Non a -> "(" ^ "¬" ^ aux a ^ ")"
    | Atome a -> a
  in
  aux f

(* ----------------- Exercice 3 : Opérateurs de simplification ----------------- *)

(** Opérateur disjonction, associatif à gauche. *)
let ( + ) f g =
  match (f, g) with
  | Bot, _ -> g
  | _, Bot -> f
  | Top, _ | _, Top -> Top
  | _ -> Ou (f, g)

(** Opérateur de conjonction, associatif à gauche. *)
let ( * ) (f : formule) (g : formule) : formule =
  match (f, g) with Bot, _ | _, Bot -> Bot | Top, Top -> Top | _ -> Et (f, g)

(** Opérateur d'implication, associatif à droite. *)
let ( ^-> ) (f : formule) (g : formule) : formule =
  match (f, g) with
  | Top, Bot -> Bot
  | Bot, _ | Top, Top -> Top
  | _ -> Imp (f, g)

(** Opérateur de négation. *)
let ( ~~ ) (f : formule) : formule =
  match f with Top -> Bot | Bot -> Top | f -> Non f

(** Simplification d'une formule. *)
let simplifier (f : formule) : formule =
  let rec aux f =
    match f with
    | Imp (f1, f2) -> Imp (aux f1, aux f2)
    | Ou (f1, f2) -> Ou (aux f1, aux f2)
    | Et (f1, f2) -> Et (aux f1, aux f2)
    | f -> f
  in
  aux f

(* ----------------- Exercice 4 : Manipulation des fonctions de construction ----------------- *)

(** Transforme une liste de formules [f1; f2; ... ; fl] en la formule f1 ∧ f2 ∧
    ... ∧ fl en considérant les éléments suivants : Si un des fi vaut Bot,
    renvoie Bot. Si un des fi vaut Top, il n'apparait pas dans le résultat. Si
    tous les fi valent Top, renvoie Top. *)
let conj_of_list (l : formule list) : formule =
  let rec aux acc l =
    match l with
    | [] -> acc
    | Top :: q -> aux acc q
    | Bot :: _ -> Bot
    | t :: q -> aux (Et (acc, t)) q
  in
  aux (List.hd l) (List.tl l)

(** Transforme une liste de formules [f1; f2; ... ; fl] en la formule f1 ∨ f2 ∨
    ... ∨ fl en considérant les éléments suivants : Si un des fi vaut Top,
    renvoie Top. Si un des fi vaut Bot, il n'apparait pas dans le résultat. Si
    tous les fi valent Bot, renvoie Bot. *)
let disj_of_list (l : formule list) : formule =
  let rec aux acc l =
    match l with
    | [] -> acc
    | Top :: _ -> Top
    | Bot :: q -> aux acc q
    | t :: q -> aux (Ou (acc, t)) q
  in
  aux (List.hd l) (List.tl l)

(** --- Exercice 5 : Fonction d'évaluation ------- *)

type interpretation = string -> bool
(** Type des interprétations. *)

(** Évalue une formule en fonction d'une interprétation. *)
let rec eval (i : interpretation) (f : formule) : bool =
  match f with
  | Imp (f1, f2) -> (not (eval i f1)) || eval i f2
  | Ou (f1, f2) -> eval i f1 || eval i f2
  | Et (f1, f2) -> eval i f1 && eval i f2
  | Equiv (f1, f2) -> eval i f1 = eval i f2 (* Équivalence: vrai si égal *)
  | Non f -> not (eval i f)
  | Atome a -> i a
  | Bot -> false
  | Top -> true

(** --- Exercice 6 : Tests de satisfaisabilité ------- *)

(** Transforme une liste de couples string en une interprétation. *)
let interpretation_of_list (l : string list) : interpretation =
 (function s -> List.mem s l)

(** Calcule la liste (triée et sans doublon) des atomes d'une formule.*)
let atomes (f : formule) : string list =
  let rec aux acc f =
    match f with
    | Imp (f1, f2) | Ou (f1, f2) | Et (f1, f2) | Equiv (f1, f2) ->
        aux (aux acc f1) f2
    | Bot | Top -> acc
    | Non a -> aux acc a
    | Atome a -> a :: acc
  in
  List.sort_uniq compare (aux [] f)

(** Calcule la liste de toutes les sous-listes d'une liste donnée. *)
let rec all_sublists (l : string list) : string list list =
  match l with
  | [] -> [ [] ]
  | x :: xs -> all_sublists xs @ List.map (fun l -> x :: l) (all_sublists xs)

(** Calcule toutes les interprétations pour une liste d'atomes donnée. *)
let all_interpretations (l : string list) : interpretation list =
  List.map interpretation_of_list (all_sublists l)

(* Détermine si une formule est satisfaisable. *)
let est_satisfaisable (l : formule) : bool =
  let atomes = atomes l in
  let interpretations = all_interpretations atomes in
  List.exists (function i -> eval i l) interpretations

(** Détermine si une formule est une tautologie. *)
let est_tautologie (l : formule) : bool =
  let atomes = atomes l in
  let interpretations = all_interpretations atomes in
  List.for_all (function i -> eval i l) interpretations

(** Détermine si une formule est une contradiction. *)
let est_contradiction (l : formule) : bool =
  let atomes = atomes l in
  let interpretations = all_interpretations atomes in
  List.for_all (function i -> not (eval i l)) interpretations

(** Détermine si une formule est contingente. *)
let est_contingente (l : formule) : bool =
  let atomes = atomes l in
  let interpretations = all_interpretations atomes in
  List.exists (function i -> eval i l) interpretations
  && List.exists (function i -> not (eval i l)) interpretations
(** ----------------- Exercice 7 : Tables de vérité ----------------- *)

type ligne = string list * bool
(** Type d'une ligne d'une table de vérité. *)

type table = ligne list
(** Type d'une table de vérité. *)

(** Calcule la table de vérité associée à une formule. *)
let table_of_formule (f : formule) : table =
  let atomes = atomes f in
  let interpretations = all_interpretations atomes in
  List.map
    (fun interpretation ->
      let valeurs = List.filter (fun a -> interpretation a) atomes in
      let result = eval interpretation f in
      (valeurs, result))
    interpretations

(** Calcule la formule associée à la dernière colonne d'une table supposée bien
    formée. *)
let formule_of_table (t : table) : formule =
  let rec aux acc l =
    match l with
    | [] -> acc
    | (at, true) :: q ->
        let conj =
          match at with
          | [] -> Top
          | [ a ] -> Atome a
          | _ -> List.fold_left (fun acc atom -> Et (Atome atom, acc)) Top at
        in
        aux (if acc = Bot then conj else Ou (acc, conj)) q
    | (_, false) :: q -> aux acc q
  in
  aux Bot t

let table_verite_with (alpha : string list) (f : formule) : table = 
  let atomes =  List.sort_uniq compare ((atomes f) @ alpha)  in
  let interpretations = all_interpretations atomes in
  List.rev(
    List.map
    (fun interpretation ->
      let valeurs = List.filter (fun a -> interpretation a) atomes in
      let result = eval interpretation f in
      (valeurs, result))
    interpretations
  )
open Formule_Syllogisme

(* open Formule_Log_Prop *)
open Proposition.Formule

(** Module des ensembles de prédicats représentés par des chaines de caractères *)
module Predicate_set = Set.Make (String)

(** Type des remplissages possibles d'un diagramme de Venn *)
type fill = Vide | NonVide

(** Module des Maps dont les clés sont des ensembles de chaines de caractères *)
module Diag = Map.Make (Predicate_set)

(** Type des diagrammes de Venn *)
type diagramme = fill Diag.t

let string_of_fill = function Vide -> "Vide" | NonVide -> "Non Vide"

let string_of_predicate_set s =
  let elements = Predicate_set.elements s in
  match elements with
  | [] ->
      "∅ "
  | elements ->
      "{" ^ String.concat ", " elements ^ "}"

(** string_of_diag d : conversion d'un diagramme d en une chaine de caractères *)
let string_of_diag (d : diagramme) : string =
  Diag.fold
    (fun pre fill acc ->
      acc ^ string_of_predicate_set pre ^ " -> " ^ string_of_fill fill ^ "\n" )
    d ""

let list_faux (s : (string list * bool) list) : string list list =
  let rec aux acc l =
    match l with
    | [] -> acc
    | (l, b) :: q when not b -> aux (acc @ [l]) q
    | (_, _) :: q -> aux acc q
  in
  aux [] s

let list_vrais (s : (string list * bool) list) : string list list =
  let rec aux acc l =
    match l with
    | [] -> acc
    | (l, b) :: q when b -> aux (acc @ [l]) q
    | (_, _) :: q -> aux acc q
  in
  aux [] s

(** diag_from_formule alpha f : construit le diagramme de Venn associé à la formule f sur
      les prédicats issus de f ou de alpha *)
let diag_from_formule (alpha : string list) (f : formule_syllogisme) :
    diagramme list =
  match f with
  | PourTout f1 ->
      let t = table_verite_with alpha f1 in
      let list_f = list_faux t in
      [ Diag.of_list
          (List.map (function a -> (Predicate_set.of_list a, Vide)) list_f) ]
  | IlExiste f1 ->
      let t = table_verite_with alpha f1 in
      let list_v = list_vrais t in
      List.map
        (function a -> Diag.of_list [(Predicate_set.of_list a, NonVide)])
        list_v

(** conj_diag d1 d2 : Calcule la combinaison/conjonction de deux diagrammes, renvoyant None si incompatibilité *)
let conj_diag (d1 : diagramme) (d2 : diagramme) : diagramme option =
  let result =
    Diag.merge
      (fun _ v1 v2 ->
        match (v1, v2) with
        | Some Vide, Some Vide -> Some (Some Vide)
        | Some NonVide, Some NonVide -> Some (Some NonVide)
        | Some Vide, Some NonVide -> Some None
        | Some NonVide, Some Vide -> Some None
        | Some v, None -> Some (Some v)
        | None, Some v -> Some (Some v)
        | None, None -> None )
      d1 d2
  in
  if Diag.exists (fun _ v -> v = None) result then None
  else Some (Diag.map (function None -> Vide | Some x -> x) result)

(** est_compatible_diag_diag dp dc : teste si le diagramme dp d'une prémisse est compatible
    avec le diagramme dc d'une conclusion *)
let est_compatible_diag_diag (dp : diagramme) (dc : diagramme) : bool =
  Diag.for_all
    (fun key v1 ->
      match Diag.find_opt key dc with
      | None -> true
      | Some v2 -> (
        match (v1, v2) with
        | Vide, Vide -> true
        | NonVide, NonVide -> true
        | Vide, NonVide -> false
        | NonVide, Vide -> false )
    )
    dp

(** est_compatible_diag_list dp dcs : teste si un diagramme dp d'une prémisse est compatible
    avec un des diagrammes de la liste dcs,
    diagrammes issus d'une conclusion *)
let est_compatible_diag_list (dp : diagramme) (dcs : diagramme list) : bool =
  List.exists (fun dc -> est_compatible_diag_diag dp dc) dcs

(** est_compatible_list_list dps dcs : teste si chacun des diagrammes de dps, diagrammes issus de prémisses,
    est compatible avec au moins un des diagrammes de dcs, diagrammes issus d'une conclusion *)
let est_compatible_list_list (dps : diagramme list) (dcs : diagramme list) :
    bool =
  List.for_all (fun dp -> est_compatible_diag_list dp dcs) dps

(** est_compatible_premisses_conc ps c : teste si une liste de prémisses ps est compatible avec une conclusion c *)
let est_compatible_premisses_conc (ps : formule_syllogisme list)
    (c : formule_syllogisme) : bool =
  let dps = List.concat (List.map (function p -> diag_from_formule [] p) ps) in
  let dcs = diag_from_formule [] c in
  est_compatible_list_list dps dcs

(** temoin_incompatibilite_premisses_conc_opt ps c : renvoie un diagramme de la combinaison des 
      prémisses ps incompatible avec la conclusion c s'il existe, None sinon *)
let temoin_incompatibilite_premisses_conc_opt (ps : formule_syllogisme list)
    (c : formule_syllogisme) : diagramme option =
  let dps = ps
    |> List.map (fun p -> diag_from_formule [] p)
    |> List.concat
  in
  let dcs = diag_from_formule [] c in
  List.find_opt (fun dp -> not (est_compatible_diag_list dp dcs)) dps

(** temoins_incompatibilite_premisses_conc ps c : renvoie les diagrammes de la combinaison
    des prémisses ps incompatibles avec la conclusion c *)
let temoins_incompatibilite_premisses_conc (ps : formule_syllogisme list)
    (c : formule_syllogisme) : diagramme list =
  let dps = ps 
    |> List.map (fun p -> diag_from_formule [] p) 
    |> List.concat
  in
  let dcs = diag_from_formule [] c in
  List.filter (fun dp -> not (est_compatible_diag_list dp dcs)) dps


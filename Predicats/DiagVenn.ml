open Formule_Syllogisme
open Proposition.Formule

module Predicate_set = Set.Make (String)
(** Module des ensembles de prédicats représentés par des chaines de caractères
*)

(** Type des remplissages possibles d'un diagramme de Venn *)
type fill = Vide | NonVide

module Diag = Map.Make (Predicate_set)
(** Module des Maps dont les clés sont des ensembles de chaines de caractères *)

type diagramme = fill Diag.t
(** Type des diagrammes de Venn *)

let string_of_fill = function
  | Vide -> "Vide" ^ "\n"
  | NonVide -> "Non Vide" ^ "\n"

let string_of_predicate_set s =
  let elements = Predicate_set.elements s in
  match elements with
  | [] -> "∅ "
  | elements -> "{" ^ String.concat ", " elements ^ "}"

(** string_of_diag d : conversion d'un diagramme d en une chaine de caractères
*)
let string_of_diag (d : diagramme) : string =
  let f k fill acc =
    match (fill, k) with
    | Vide, k1 when Predicate_set.to_list k1 = [] -> "∅ -> Vide\n" ^ acc
    | NonVide, k1 when Predicate_set.to_list k1 = [] -> "∅ -> NonVide\n" ^ acc
    | Vide, k1 ->
        "{" ^ String.concat "," (Predicate_set.to_list k1) ^ "} -> Vide\n" ^ acc
    | NonVide, k1 ->
        "{"
        ^ String.concat "," (Predicate_set.to_list k1)
        ^ "} -> Non Vide\n" ^ acc
  in
  Diag.fold f d ""

let list_faux (s : (string list * bool) list) : string list list =
  let rec aux acc l =
    match l with
    | [] -> acc
    | (l, b) :: q when not b -> aux (acc @ [ l ]) q
    | (_, _) :: q -> aux acc q
  in
  aux [] s

let list_vrais (s : (string list * bool) list) : string list list =
  let rec aux acc l =
    match l with
    | [] -> acc
    | (l, b) :: q when b -> aux (acc @ [ l ]) q
    | (_, _) :: q -> aux acc q
  in
  aux [] s

(** diag_from_formule alpha f : construit le diagramme de Venn associé à la
    formule f sur les prédicats issus de f ou de alpha *)
let diag_from_formule (alpha : string list) (f : formule_syllogisme) :
    diagramme list =
  match f with
  | PourTout f1 ->
      let t = table_verite_with alpha f1 in
      let list_f = list_faux t in
      [
        Diag.of_list
          (List.map (function a -> (Predicate_set.of_list a, Vide)) list_f);
      ]
  | IlExiste f1 ->
      let t = table_verite_with alpha f1 in
      let list_v = list_vrais t in
      List.map
        (function a -> Diag.of_list [ (Predicate_set.of_list a, NonVide) ])
        list_v

(** conj_diag d1 d2 : Calcule la combinaison/conjonction de deux diagrammes,
    renvoyant None si incompatibilité *)
let conj_diag (d1 : diagramme) (d2 : diagramme) : diagramme option =
  let result =
    Diag.merge
      (fun _ v1 v2 ->
        match (v1, v2) with
        | Some Vide, Some Vide -> Some (Some Vide)
        | Some NonVide, Some NonVide -> Some (Some NonVide)
        | Some Vide, Some NonVide -> Some None  (* Incompatibilité *)
        | Some NonVide, Some Vide -> Some None  (* Incompatibilité *)
        | Some v, None -> Some (Some v)
        | None, Some v -> Some (Some v)
        | None, None -> None)
      d1 d2
  in
  (* Vérifier s'il y a des incompatibilités (valeurs None) *)
  if Diag.exists (fun _ v -> v = None) result then 
    None
  else 
    (* Extraire les valeurs Some, on sait qu'il n'y a pas de None *)
    Some (Diag.filter_map (fun _ v -> v) result)

(** est_compatible_diag_diag dp dc : teste si le diagramme dp d'une prémisse est
    compatible avec le diagramme dc d'une conclusion *)
let est_compatible_diag_diag (dp : diagramme) (dc : diagramme) : bool =
  let conj = conj_diag dp dc in
  if conj <> None then dc = Option.get conj else false

(** est_compatible_diag_list dp dcs : teste si un diagramme dp d'une prémisse
    est compatible avec un des diagrammes de la liste dcs, diagrammes issus
    d'une conclusion *)
let est_compatible_diag_list (dp : diagramme) (dcs : diagramme list) : bool =
  List.exists (fun dc -> est_compatible_diag_diag dp dc) dcs

(** est_compatible_list_list dps dcs : teste si chacun des diagrammes de dps,
    diagrammes issus de prémisses, est compatible avec au moins un des
    diagrammes de dcs, diagrammes issus d'une conclusion *)
let est_compatible_list_list (dps : diagramme list) (dcs : diagramme list) :
    bool =
  List.for_all (fun dp -> est_compatible_diag_list dp dcs) dps

(** est_compatible_premisses_conc ps c : teste si une liste de prémisses ps est
    compatible avec une conclusion c *)
let est_compatible_premisses_conc (ps : formule_syllogisme list)
    (c : formule_syllogisme) : bool =
  let dps =
    List.concat (List.map (function p -> diag_from_formule [] p) ps)
  in
  let dcs = diag_from_formule [] c in
  est_compatible_list_list dps dcs

(** atomes_syl ps : renvoie la liste des atomes de la combinaison des premisses
    ps *)
let atomes_syl (ps : formule_syllogisme list) : string list =
  List.sort_uniq String.compare
    (List.fold_left
       (fun acc p -> match p with IlExiste f | PourTout f -> acc @ atomes f)
       [] ps)

(** conj_diag_list ds1 ds2 renvoie la conjonction de deux listes de diagrammes
    ds1 et ds2 *)
let conj_diag_list (ds1 : diagramme list) (ds2 : diagramme list) : diagramme list =
  let cartesian_product = List.concat (List.map (fun a -> List.map (fun b -> (a, b)) ds2) ds1) in
  List.filter_map
    (fun (d1, d2) ->
      match conj_diag d1 d2 with
      | None -> None
      | Some combined_diag -> Some combined_diag
    )
    cartesian_product

(** temoin_incompatibilite_premisses_conc_opt ps c : renvoie un diagramme de la
    combinaison des prémisses ps incompatible avec la conclusion c s'il existe,
    None sinon *)
let temoins_incompatibilite_premisses_conc_opt (ps : formule_syllogisme list)
    (c : formule_syllogisme) : diagramme option =
  match ps with
  | [] -> None (* Pas de prémisses = pas de témoin *)
  | _ ->
      let at_syl = atomes_syl ps in
      let dps = List.map (fun p -> diag_from_formule at_syl p) ps in
      let dcs = diag_from_formule [] c in

      let comb_dps =
        match dps with
        | [] -> []
        | hd :: tl -> List.fold_left (fun acc dp -> conj_diag_list acc dp) hd tl
      in

      List.find_opt (fun dp -> not (est_compatible_diag_list dp dcs)) comb_dps

(** temoins_incompatibilite_premisses_conc ps c : renvoie les diagrammes de la
    combinaison des prémisses ps incompatibles avec la conclusion c *)
let temoins_incompatibilite_premisses_conc (ps : formule_syllogisme list)
    (c : formule_syllogisme) : diagramme list =
  let at_syl = atomes_syl ps in
  let dps = List.map (fun p -> diag_from_formule at_syl p) ps in
  let dcs = diag_from_formule [] c in
  let comb_dps =
    List.fold_left
      (fun acc dp -> conj_diag_list acc dp)
      (List.hd dps) (List.tl dps)
  in
  List.filter (fun dp -> not (est_compatible_diag_list dp dcs)) comb_dps

(* ***** Ajouts pour le projet ***** *)

let neg_reg (reg: fill) : fill = match reg with
  | NonVide -> Vide
  | Vide -> NonVide

(** negate_diag d renvoie la négation du diagramme d*)
let negate_diag (d : diagramme) : diagramme list =
  if Diag.is_empty d then []
  else Diag.fold (fun k v acc -> (Diag.singleton k (neg_reg v))::acc ) d []
  
(** negate_diag_list ds renvoie la négation de la liste de diagrammes ds *)
let negate_diag_list (ds : diagramme list) : diagramme list =
  match ds with
  | [] -> [ Diag.empty ]
  | ds -> 
      let neg = List.map negate_diag ds in
      match neg with
      | [] -> []
      | hd :: tl -> List.fold_left conj_diag_list hd tl

(** disj_of_diag_list ds1 ds2 renvoie la disjonction de deux listes de
    diagrammes ds1 et ds2 *)
let disj_of_diag_list (ds1 : diagramme list) (ds2 : diagramme list) :
    diagramme list =
  ds1 @ ds2

(** atomes_of_bool_comb b renvoie la liste des atomes associée à b **)
let rec atomes_of_bool_comb (b : boolCombSyllogismes) : string list =
  match b with
  | Vrai -> []
  | Faux -> []
  | Base f -> ( match f with PourTout x -> atomes x | IlExiste x -> atomes x)
  | Et (a, b) -> atomes_of_bool_comb a @ atomes_of_bool_comb b
  | Ou (a, b) -> atomes_of_bool_comb a @ atomes_of_bool_comb b
  | Non a -> atomes_of_bool_comb a

(** diags_of_bool_comb alpha b renvoie la liste des diagrammes associés à la
    combinaison booléenne b de formules pour syllogismes, sur les prédicats
    issus de b ou de alpha *)
let diags_of_bool_comb (alpha : string list) (b : boolCombSyllogismes) :
    diagramme list =
  let all_atomes = List.sort_uniq compare (alpha @ atomes_of_bool_comb b) in
  let rec aux b =
    match b with
    | Vrai -> [ Diag.empty ]
    | Faux -> []
    | Base f -> diag_from_formule all_atomes f
    | Et (b1, b2) -> conj_diag_list (aux b1) (aux b2)
    | Ou (b1, b2) -> disj_of_diag_list (aux b1) (aux b2)
    | Non b' -> negate_diag_list (aux b')
  in
  aux b

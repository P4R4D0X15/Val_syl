open Formule_Syllogisme
open Proposition.Formule
(* open Formule_Log_Prop *)

module Predicate_set = Set.Make (String)
(** Module des ensembles de prédicats représentés par des chaines de caractères *)

(** Type des remplissages possibles d'un diagramme de Venn *)
type fill = Vide | NonVide

module Diag = Map.Make (Predicate_set)
(** Module des Maps dont les clés sont des ensembles de chaines de caractères *)

type diagramme = fill Diag.t
(** Type des diagrammes de Venn *)

(** string_of_diag d : conversion d'un diagramme d en une chaine de caractères *)
let string_of_diag (d : diagramme) : string = 
  let f k fill acc = match fill, k with
    | Vide , k1 when Predicate_set.to_list k1 = [] -> "∅ -> Vide\n" ^ acc
    | NonVide, k1 when Predicate_set.to_list k1 = [] -> "∅ -> NonVide\n" ^ acc
    | Vide , k1-> "{" ^ (String.concat "," (Predicate_set.to_list k1)) ^ "} -> Vide\n"^acc 
    | NonVide , k1 -> "{" ^ (String.concat ","(Predicate_set.to_list k1)) ^ "} -> Non Vide\n"^acc 
  in  Diag.fold f d ""


let string_of_diag_list (dl : diagramme list) : string = List.fold_left (fun acc diag -> "Diagramme :\n"^(string_of_diag diag)^acc) "" dl

(** diag_from_formule alpha f : construit le diagramme de Venn associé à la formule f sur
      les prédicats issus de f ou de alpha *)
let diag_from_formule (s : string list) (fs : formule_syllogisme) : diagramme list =
  let g acc slb = Diag.add (Predicate_set.of_list (fst slb)) Vide acc and
  ng acc slb = (Diag.add (Predicate_set.of_list (fst slb)) NonVide Diag.empty)::acc in
  match fs with
  | PourTout f ->  let table = table_verite_with s f in [List.fold_left g Diag.empty (List.filter (fun a -> not (snd a)) table)]
  | IlExiste f ->  let table = table_verite_with s f in List.fold_left ng [] (List.filter (fun a -> (snd a)) table)

(** conj_diag d1 d2 : Calcule la combinaison/conjonction de deux diagrammes, renvoyant None si incompatibilité *)
let conj_diag (d1 : diagramme) (d2 : diagramme) : diagramme option = 
  let acc = Diag.merge (
    fun _ a b -> match a ,b with
      | Some a0, Some b0 when a0 = b0 -> Some a0
      | _, _ -> None
  ) d1 d2 in 
  if acc <> Diag.empty then Some acc else None

(** est_compatible_diag_diag dp dc : teste si le diagramme dp d'une prémisse est compatible
    avec le diagramme dc d'une conclusion *)
let est_compatible_diag_diag (dp : diagramme) (dc : diagramme) : bool =
  dc = Option.get(conj_diag dp dc)

(** est_compatible_diag_list dp dcs : teste si un diagramme dp d'une prémisse est compatible
    avec un des diagrammes de la liste dcs,
    diagrammes issus d'une conclusion *)
let est_compatible_diag_list (dp : diagramme) (dcs : diagramme list) : bool =
  List.exists (fun dc -> est_compatible_diag_diag dp dc) dcs

(** est_compatible_list_list dps dcs : teste si chacun des diagrammes de dps, diagrammes issus de prémisses,
    est compatible avec au moins un des diagrammes de dcs, diagrammes issus d'une conclusion *)
let est_compatible_list_list (dps : diagramme list) (dcs : diagramme list) : bool =
  List.for_all (fun dp -> est_compatible_diag_list dp dcs) dps

(** est_compatible_premisses_conc ps c : teste si une liste de prémisses ps est compatible avec une conclusion c *)
let est_compatible_premisses_conc (fsl : formule_syllogisme list)
    (c : formule_syllogisme) : bool = 
  List.for_all (fun fs -> est_compatible_list_list (diag_from_formule [] fs) (diag_from_formule [] c)) fsl

(** temoin_incompatibilite_premisses_conc_opt ps c : renvoie un diagramme de la combinaison des 
    prémisses ps incompatible avec la conclusion c s'il existe, None sinon *)
let temoin_incompatibilite_premisses_conc_opt (_ : formule_syllogisme list)
    (_ : formule_syllogisme) : diagramme option = failwith "afer"
  (*let aux = (List.find (not (est_compatible_premisses_conc fsl c)) fsl) in*)

(** temoins_incompatibilite_premisses_conc ps c : renvoie les diagrammes de la combinaison
    des prémisses ps incompatibles avec la conclusion c *)
let temoins_incompatibilite_premisses_conc (_ : formule_syllogisme list)
    (_ : formule_syllogisme) : diagramme list =
  failwith "à faire"

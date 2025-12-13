open DiagVenn
open Formule_Syllogisme
open Proposition.Formule

(** all_prem_sublist : Construit la liste des Predicate.Set correspondante à
    toutes les sous listes de ats *)
let all_prem_sublist (ats : string list) : Predicate_set.t list =
  List.map
    (fun at ->
      List.fold_left
        (fun acc a -> Predicate_set.add a acc)
        Predicate_set.empty at)
    (all_sublists ats)

(** undef d : supprime dans pl toutes les zones qui sont définies dans d *)
let undef (d : diagramme) (pl : Predicate_set.t list) : Predicate_set.t list =
  let rec aux acc pl =
    match pl with
    | [] -> acc
    | p :: q when Diag.mem p d -> aux acc q
    | p :: q -> aux (p :: acc) q
  in
  aux [] pl

(** diag_from_undef d p : construit le liste des extensions possible du
    diagramme d sur une zone p*)
let diag_from_undef (d : diagramme) (p : Predicate_set.t) : diagramme list =
  [ Diag.add p NonVide d; Diag.add p Vide d ]

(** complete_diags d ats renvoie la liste des extensions complètes de d en
    considérant les atomes de la liste ats pour considérer les zones à
    compléter. Par exemple, un diagramme défini par une contrainte vide sur une
    zone A pourrait être complété en considérant les zones définies par une
    liste d'atomes [A; B; C]. *)
let complete_diags (d : diagramme) (ats : string list) : diagramme list =
  let ud = undef d (all_prem_sublist ats) in
  let d_list = List.map (fun p -> diag_from_undef d p) ud in
  List.fold_left
    (fun acc dl -> conj_diag_list acc dl)
    (List.hd d_list) (List.tl d_list)

(** is_contradiction d1 d2 teste si les diagrammes d1 et d2 sont en
    contradiction, c'est-à-dire s'il existe une zone non-vide de d1 qui est vide
    dans d2 ou inversement *)
let is_contradiction (d1 : diagramme) (d2 : diagramme) : bool =
  Diag.exists (fun pre v -> Diag.find pre d2 <> v) d1

(** est_valid_premiss_conc b1 b2 teste si pour deux combinaisons booléennes de
    formules pour syllogismes b1 et b2, b1 valide b2*)
let est_valid_premiss_conc (b1 : boolCombSyllogismes) (b2 : boolCombSyllogismes)
    : bool =
  let db1 =
    List.concat
      (List.map (fun d -> complete_diags d []) (diags_of_bool_comb [] b1))
  and db2 =
    List.concat
      (List.map (fun d -> complete_diags d []) (diags_of_bool_comb [] b2))
  in
  not (List.exists(fun d2 -> List.for_all (fun d1 -> is_contradiction d1 d2) db2) db1)

(** temoins_invalidite_premisses_conc b1 b2 renvoie les diagrammes de la
    combinaison des prémisses b1 invalidant la conclusion b2 *)
let temoins_invalidite_premisses_conc (b1 : boolCombSyllogismes)
    (b2 : boolCombSyllogismes) : diagramme list =
  let db1 =
    List.concat
      (List.map (fun d -> complete_diags d []) (diags_of_bool_comb [] b1))
  and db2 =
    List.concat
      (List.map (fun d -> complete_diags d []) (diags_of_bool_comb [] b2))
  in
  List.find_all
    (fun d1 -> List.for_all (fun d2 -> is_contradiction d1 d2) db2)
    db1

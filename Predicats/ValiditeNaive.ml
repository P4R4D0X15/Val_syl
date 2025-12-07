open DiagVenn
open Formule_Syllogisme
open Proposition.Formule

(** complete_diags d ats renvoie la liste des extensions complètes de d en
    considérant les atomes de la liste ats pour considérer les zones à
    compléter. Par exemple, un diagramme défini par une contrainte vide sur une
    zone A pourrait être complété en considérant les zones définies par une
    liste d'atomes [A; B; C]. *)
let complete_diags (d : diagramme) (ats : string list) : diagramme list =
  let pred =
    List.map
      (fun at ->
        List.fold_left
          (fun acc a -> Predicate_set.add a acc)
          Predicate_set.empty at)
      (all_sublists ats)
  in
  List.iter (fun p -> print_endline (string_of_predicate_set p)) pred;
  List.fold_left
    (fun diag p ->
      if not (List.exists (fun di -> Diag.mem p di) diag) then
        Diag.add p NonVide Diag.empty :: diag
      else diag)
    [ d ] pred

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
  List.exists
    (fun d1 -> List.exists (fun d2 -> not (is_contradiction d1 d2)) db2)
    db1

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
    (fun d1 -> List.exists (fun d2 -> not (is_contradiction d1 d2)) db2)
    db1

open DiagVenn
open Formule_Syllogisme

(** complete_diags d ats renvoie la liste des extensions complètes de d en
    considérant les atomes de la liste ats pour considérer les zones à
    compléter. Par exemple, un diagramme défini par une contrainte vide sur une
    zone A pourrait être complété en considérant les zones définies par une
    liste d'atomes [A; B; C]. *)
let complete_diags (_ : diagramme) (_ : string list) : diagramme list =
  failwith "complete_diags : à faire"

(** is_contradiction d1 d2 teste si les diagrammes d1 et d2 sont en
    contradiction, c'est-à-dire s'il existe une zone non-vide de d1 qui est vide
    dans d2 ou inversement *)
let is_contradiction (_ : diagramme) (_ : diagramme) : bool =
  failwith "is_contradiction : à faire"

(** est_valid_premiss_conc b1 b2 teste si pour deux combinaisons booléennes de
    formules pour syllogismes b1 et b2, b1 valide b2*)
let est_valid_premiss_conc (_ : boolCombSyllogismes) (_ : boolCombSyllogismes) :
    bool =
  failwith "est_valid_premiss_conc : à faire"

(** temoins_invalidite_premisses_conc b1 b2 renvoie les diagrammes de la
    combinaison des prémisses b1 invalidant la conclusion b2 *)
let temoins_invalidite_premisses_conc (_ : boolCombSyllogismes)
    (_ : boolCombSyllogismes) : diagramme list =
  failwith "temoins_invalidite_premisses_conc : à faire"

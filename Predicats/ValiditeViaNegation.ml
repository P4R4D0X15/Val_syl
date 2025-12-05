open DiagVenn
open Formule_Syllogisme

(** est_valid_premiss_conc b1 b2 teste si pour deux combinaisons booléennes de
    formules pour syllogismes b1 et b2, b1 valide b2, en utilisant la méthode de
    la conjonction avec les diagrammes inverses de la conclusion *)
let est_valid_premiss_conc' (_ : boolCombSyllogismes) (_ : boolCombSyllogismes)
    : bool =
  failwith "est_valid_premiss_conc' : à faire"

(** temoins_invalidite_premisses_conc' b1 b2 renvoie les diagrammes de la
    conjonction des diagrammes de b1 avec la négation de b2, qui contredisent
    chaque diagramme de b2 *)
let temoins_invalidite_premisses_conc' (_ : boolCombSyllogismes)
    (_ : boolCombSyllogismes) : diagramme list =
  failwith "temoins_invalidite_premisses_conc' : à faire"

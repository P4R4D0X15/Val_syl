open Proposition.Formule

(** Type des formules utilisées pour les syllogismes *)
type formule_syllogisme = PourTout of formule | IlExiste of formule

(** string_of_formule_log_prop_var s f : conversion d'une formule f en chaîne de caractères,
    en les représentant comme des prédicats unaires appliqués sur des 
    occurrences de la variable s. *)
let string_of_formule_log_prop_var (x : string) (f : formule) : string =
  let rec aux f =
    match f with
    | Imp (f1, f2) -> "(" ^ aux f1 ^ " -> " ^ aux f2 ^ ")"
    | Ou (f1, f2) -> "(" ^ aux f1 ^ " v " ^ aux f2 ^ ")"
    | Et (f1, f2) -> "(" ^ aux f1 ^ " ∧ " ^ aux f2 ^ ")"
    | Equiv (f1, f2) -> "(" ^ aux f1 ^ " <=> " ^ aux f2 ^ ")"
    | Bot -> "⊥"
    | Top -> "T"
    | Non a -> "¬" ^ aux a 
    | Atome a -> a ^ "("^ x ^")"
  in
  aux f

(** string_of_formule_syllogisme f : conversion d'une formule f en chaîne de caractères,
    en considérant des prédicats unaires appliqués sur des 
    occurrences de la variable s. *)
let string_of_formule_syllogisme (fs : formule_syllogisme) : string =
  match fs with
  | PourTout f -> "∀x " ^ string_of_formule_log_prop_var "x" f
  | IlExiste f -> "∃x " ^ string_of_formule_log_prop_var "x" f

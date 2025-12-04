(** Un terme est soit une variable, soit une combinaison d'un symbole de
    fonction et d'une liste de termes. *)
type terme = Var of string | Function of string * terme list

(** Transforme un terme en une chaine de caractères. *)
let rec string_of_terme (t : terme) : string = 
  match t with
  | Var s -> s
  | Function (s, tl) -> s^"("^(String.concat "," (List.fold_left (fun acc terme -> (string_of_terme terme)::acc) [] tl))^")"

(** Type des formules quantifiées du calcul des prédicats. *)
type formule =
  | Bot  (** La formule fausse. *)
  | Top  (** La formule vraie. *)
  | Atome of string * terme list
    (* Un symbole de prédicat appliqué sur une liste de termes. *)
  | Non of formule  (** La négation d'une formule. *)
  | Ou of formule * formule  (** La disjonction de deux formules. *)
  | Et of formule * formule  (** La conjonction de deux formules. *)
  | Imp of formule * formule  (** L'implication de deux formules. *)
  | IlExiste of string * formule
      (** La quantification existentielle d'une formule. *)
  | PourTout of string * formule
      (** La quantification universelle d'une formule. *)

(** Transforme une formule en une chaine de caractères. *)
let rec string_of_formule (f : formule) : string = 
  match f with
    | Bot -> "⊥"
    | Top -> "T"
    | Atome (s, tl) -> (String.concat s (List.rev(List.fold_left (fun acc terme -> (string_of_terme terme)::acc) [] tl)))
    | Imp (f1, f2) -> "(" ^ (string_of_formule f1) ^ " -> " ^ (string_of_formule f2) ^ ")"
    | Ou (f1, f2) -> "(" ^ (string_of_formule f1) ^ " v " ^ (string_of_formule f2) ^ ")"
    | Et (f1, f2) -> "(" ^ (string_of_formule f1) ^ " ∧ " ^ (string_of_formule f2) ^ ")"
    | Non g -> "(" ^ "¬" ^ (string_of_formule g) ^ ")"
    | PourTout (s,g) -> "∀"^s^string_of_formule g   
    | IlExiste (s,g) -> "∃"^s^string_of_formule g



module StringSet = Set.Make (String)
(** Module des ensembles de chaines de caractères. *)

(** Renvoie l'ensemble des variables d'un terme. *)
let variables (t : terme) : StringSet.t = 
  let rec aux acc terme = 
    match terme with
    | Var x -> x::acc
    | Function (_, tl) -> List.fold_left (fun ac ter -> (aux ac ter)@ac) acc tl
  in StringSet.of_list(aux [] t)

(** Renvoie l'ensemble des variables libres d'une formule. *)
let variables_libres (f : formule) : StringSet.t = 
  let rec aux acc g =
    match g with
    | Bot | Top -> acc
    | Atome (_, tl) -> StringSet.union acc (StringSet.of_list((List.concat_map (fun t -> StringSet.to_list(variables t)) tl)))
    | Imp (f1, f2) | Et (f1, f2) | Ou (f1, f2) -> StringSet.union (aux acc f1) (aux acc f2)
    | Non h -> StringSet.union acc (aux acc h)
    | PourTout (s, h) | IlExiste(s, h) -> StringSet.union (StringSet.remove s (aux acc h)) acc
  in aux StringSet.empty f

  (** Renvoie l'ensemble des variables liées d'une formule. *)
let rec variables_liees (f : formule) : StringSet.t = 
  match f with
  | Bot | Top -> StringSet.empty
  | Atome (_, tl) -> StringSet.union acc (StringSet.of_list((List.concat_map (fun t -> StringSet.to_list(variables t)) tl)))
  | Imp (f1, f2) | Et (f1, f2) | Ou (f1, f2) -> StringSet.union (aux acc f1) (aux acc f2)
  | Non h -> variables_liees h
  | PourTout (s, h) | IlExiste(s, h) -> StringSet.union (StringSet.remove s (aux acc h)) acc
  

type 'a interpretation = {
  assoc_fun : string -> 'a list -> 'a option;
  assoc_pred : string -> 'a list -> bool option;
}
(** Une interprétation sur un domaine 'a associe
    - à chaque symbole de fonction une fonction 
      transformant une liste de 'a en 'a option (None s'il y a un problème d'arité)
    - à chaque symbole de prédicat une fonction 
      transformant une liste de 'a en Booléen optionnel (None s'il y a un problème d'arité). *)

type 'a realisation = string -> 'a
(** Une réalisation sur un domaine 'a associe à chaque 
    variable une valeur de type 'a. *)

(** Transforme une liste de 'a option :
    - en une valeur Some [v1; ... vn] si la liste est [Some v1; ...; Some vn]
    - en None si un élément de la liste vaut None. *)
let sequence (_ : 'a option list) : 'a list option = failwith "à faire"

(** Évalue un terme à l'aide d'une interprétation et d'une réalisation.
    Renvoie None si un problème d'arité est rencontré. *)
let safe_term_eval (_ : 'a interpretation) (_ : 'a realisation) (_ : terme) :
    'a option =
  failwith "à faire"

(** Foncteur permettant de créer une fonction d'évaluation d'une formule à l'aide d'un type énumérable,
    afin d'énumérer toutes les valeurs lors d'une quantification de variable. *)
module MakeEval (M : Enumerable.Enumerable) = struct
  (** Évalue une formule à l'aide d'une interprétation et d'une réalisation.
      Renvoie None si un problème d'arité est rencontré. *)
  let safe_eval (_ : M.t interpretation) (_ : M.t realisation) (_ : formule) :
      bool option =
    failwith "à faire"
end

open DiagVenn
open Formule_Syllogisme
open Proposition.Formule

let a = Atome "a"
let b = Atome "b"
let c = Atome "c"
let p1 = PourTout (Imp (Ou (a, b), c))
let p2 = PourTout (Imp (c, Ou (a, b)))
let p3 = IlExiste a
let c1 = IlExiste b

let test (ps : formule_syllogisme list) (c : formule_syllogisme) : unit =
  print_endline "Prémisses :";
  List.iter (fun p -> print_endline (string_of_formule_syllogisme p)) ps;

  print_endline "Conclusion :";
  print_endline (string_of_formule_syllogisme c);

  let dps = List.map (fun p -> diag_from_formule ["a"; "b"; "c"] p) ps in
  print_endline "Diagramme de chaque prémisse :";
  List.iter (fun d -> print_endline (string_of_diag d)) (List.concat dps);

  let comb_dps = List.fold_left (fun acc dp -> conj_diag_list acc dp) (List.hd dps) (List.tl dps) in
  print_endline "Diagrammes de la combinaison :";
  List.iter (fun d -> print_endline (string_of_diag d)) comb_dps;

  let dcs = diag_from_formule ["a"; "b"; "c"] c in
  print_endline "Diagrammes de la conclusion :";
  List.iter (fun d -> print_endline (string_of_diag d)) dcs;
  
  let compatible = est_compatible_list_list comb_dps dcs in

  if compatible then print_endline "Les prémisses sont compatibles avec la conclusion."
  else let contre_exemples = temoins_incompatibilite_premisses_conc ps c in
  print_endline
    "Conclusion incompatible avec les diagrammes, contre-exemples :";
  List.iter (fun d -> print_endline (string_of_diag d)) contre_exemples


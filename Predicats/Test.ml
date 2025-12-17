open DiagVenn
open Proposition.Formule
open Formule_Syllogisme
open ValiditeNaive
open ValiditeViaNegation


let a = Atome "a"
let b = Atome "b"
let c = Atome "c"
let p1 = PourTout (Imp (Ou (a, b), c))
let p2 = PourTout (Imp (c, Ou (a, b)))
let p3 = IlExiste a
let c1 = IlExiste b

(* b1 : pour tout x, a(x) ou b(x)*)
let b1 = Base (PourTout (Ou (Atome "a", Atome "b")))
(* b2 : (pour tout x, a(x)) ou (pour tout x, b(x)) *)
let b2 = Ou (Base (PourTout (Atome "a")), Base (PourTout (Atome "b")))

let test (ps : formule_syllogisme list) (c : formule_syllogisme) : unit =
  print_endline "Prémisses :";
  List.iter (fun p -> print_endline (string_of_formule_syllogisme p)) ps;

  print_endline "Conclusion :";
  print_endline (string_of_formule_syllogisme c);

  let dps = List.map (fun p -> diag_from_formule [ "a"; "b"; "c" ] p) ps in
  print_endline "Diagramme de chaque prémisse :";
  List.iter (fun d -> print_endline (string_of_diag d)) (List.concat dps);

  let comb_dps =
    List.fold_left
      (fun acc dp -> conj_diag_list acc dp)
      (List.hd dps) (List.tl dps)
  in
  print_endline "Diagrammes de la combinaison :";
  List.iter (fun d -> print_endline (string_of_diag d)) comb_dps;

  let dcs = diag_from_formule [ "a"; "b"; "c" ] c in
  print_endline "Diagrammes de la conclusion :";
  List.iter (fun d -> print_endline (string_of_diag d)) dcs;

  let compatible = est_compatible_list_list comb_dps dcs in

  if compatible then
    print_endline "Les prémisses sont compatibles avec la conclusion."
  else
    let contre_exemples = temoins_incompatibilite_premisses_conc ps c in
    print_endline
      "Conclusion incompatible avec les diagrammes, contre-exemples :";
    List.iter (fun d -> print_endline (string_of_diag d)) contre_exemples;

    print_endline "Test Complete Diags:\n";
    (* let complete_dps = List.map (fun dp -> complete_diags dp ["a";"b";"c"]) (List.concat dps) in
    List.iter (fun d -> print_endline (string_of_diag (d))) (List.concat complete_dps) *)
    let compl_dp1 = List.map (fun dp -> complete_diags dp ["a";"b";"c"]) (diag_from_formule ["a";"b";"c"] p1) in
    print_endline "-p1:\n";
    List.iter (fun d -> print_endline (string_of_diag (d))) (diag_from_formule ["a";"b";"c"] p1);
    print_endline "-Complete diag sur p1:\n";
    List.iter (fun d -> print_endline (string_of_diag (d))) (List.concat compl_dp1);;

let test_boolCombSyllogisme (b1 : boolCombSyllogismes) (b2 : boolCombSyllogismes) : unit =
  let db1s = diags_of_bool_comb [] b1 in 
  let db2s = diags_of_bool_comb [] b2 in
  print_endline "====== b1 = pour tout x, a(x) ou b(x) =======\n ";
  List.iter (fun db1 -> print_endline (string_of_diag db1)) db1s; 
  print_endline "====== b2 = (pour tout x, a(x)) ou (pour tout x, b(x)) =======\n ";
  List.iter (fun db2 -> print_endline (string_of_diag db2)) db2s;
  let resultNaive = est_valid_premiss_conc b1 b2 in 
  let resultNegation = est_valid_premiss_conc' b1 b2 in 
  print_endline ("======Validité Naive ======\nb1 valide b2 ? "^(string_of_bool resultNaive) ^"\n");

  print_endline ("======Validité Via Negation ======\nb1 valide b2 ? "^(string_of_bool resultNegation )^"\n");
  print_endline ("====== Temoins invalidite premisses conc naive ====== ");
  if not (resultNaive) then List.iter (fun dp -> print_endline(string_of_diag dp)) (temoins_invalidite_premisses_conc b1 b2); 
  print_endline ("====== Temoins invalidite premisses conc via négation ====== ");
  if not (resultNegation) then List.iter (fun dp -> print_endline(string_of_diag dp)) (temoins_invalidite_premisses_conc' b1 b2)
  ;;

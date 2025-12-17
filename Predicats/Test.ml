open OUnit2
open Formule_Syllogisme

(* p1 = ∀x (b(x) → a(x)) *)
let p1 = Base (PourTout (Imp (Atome "b", Atome "a")))

(* p2 = ∀x (a(x) → b(x)) *)
let p2 = Base (PourTout (Imp (Atome "a", Atome "b")))

(* p3 = ∃x (a(x) ∧ ¬b(x)) *)
let p3 = Base (IlExiste (Et (Atome "a", Non (Atome "b"))))

(* p4 = ∀x (a(x) ∨ b(x)) *)
let p4 = Base (PourTout (Ou (Atome "a", Atome "b")))

(* p5 = ∀x (a(x)) *)
let p5 = Base (PourTout (Atome "a"))

(* p6 = ∀x (b(x)) *)
let p6 = Base (PourTout (Atome "b"))

(* p7 = ∃x (a(x)) *)
(* let p7 = Base (IlExiste (Atome "a")) *)

(* p8 = ∃x (b(x)) *)
(* let p8 = Base (IlExiste (Atome "b")) *)

let string_eq (a : string) (b : string) = assert_equal a b ~printer:(fun a -> a)

let bool_eq (a : bool) (b : bool) =
  assert_equal a b ~printer:(fun a -> string_of_bool a)

module ValiditeNaiveTest = struct
  open ValiditeNaive
  open DiagVenn

  let tests =
    "test suite for eval"
    >::: [
           (* ( "complete_diags" >:: fun _ -> *)
             (* let d = Diag.empty in *)
             (* let ats = [ "a" ] in *)
             (* let result = complete_diags d ats in *)
             (* string_eq *)
               (* "∅->NonVide,{a}->NonVide|∅->NonVide,{a}->Vide|∅->Vide,{a}->NonVide|∅->Vide,{a}->Vide" *)
               (* (String.concat "|" (List.map (fun s -> string_of_diag s) result)) *)
           (* ); *)
           ( "complete_diags2" >:: fun _ ->
             let d = Diag.singleton Predicate_set.empty Vide in
             let ats = [ "a" ] in
             let result = complete_diags d ats in
             string_eq "{a} -> Vide\n∅ -> NonVide\n\n{a} -> Vide\n∅ -> Vide\n"
               (String.concat "\n" (List.map (fun s -> string_of_diag s) (List.rev result)))
           );
           ( "complete_diags3" >:: fun _ ->
             let d = Diag.singleton (Predicate_set.singleton "a") Vide in
             let ats = [ "a"; "b" ] in
             let result = complete_diags d ats in
             string_eq
               "{b} -> NonVide\n{a,b} -> NonVide\n{a} -> Vide\n∅ -> NonVide\n\n{b} -> NonVide\n{a,b} -> Vide\n{a} -> Vide\n∅ -> NonVide\n\n{b} -> Vide\n{a,b} -> NonVide\n{a} -> Vide\n∅ -> NonVide\n\n{b} -> Vide\n{a,b} -> Vide\n{a} -> Vide\n∅ -> NonVide\n\n{b} -> NonVide\n{a,b} -> NonVide\n{a} -> Vide\n∅ -> Vide\n\n{b} -> NonVide\n{a,b} -> Vide\n{a} -> Vide\n∅ -> Vide\n\n{b} -> Vide\n{a,b} -> NonVide\n{a} -> Vide\n∅ -> Vide\n\n{b} -> Vide\n{a,b} -> Vide\n{a} -> Vide\n∅ -> Vide\n"
               (String.concat "\n" (List.map (fun s -> string_of_diag s) (List.rev result)))
           );
           ( "complete_diags_all_complete" >:: fun _ ->
             let d =
               Diag.of_list
                 [
                   (Predicate_set.singleton "a", Vide);
                   (Predicate_set.empty, NonVide);
                 ]
             in
             let ats = [ "a" ] in
             let result = complete_diags d ats in
             string_eq "{a} -> NonVide\n∅ -> Vide\n"
               (String.concat "\n" (List.map (fun s -> string_of_diag s) (List.rev result)))
           );
         ]
end

module DiagVennTest = struct
  open DiagVenn

  let tests =
    "test suite for eval"
    >::: [

           ( "" >:: fun _ ->
             let d : diagramme =
               Diag.of_list
                 [
                   (Predicate_set.singleton "b", Vide);
                   (Predicate_set.empty, Vide);
                 ]
             in
             let n = negate_diag d in
             string_eq "{b} -> NonVide\n\n∅ -> NonVide\n"
               (String.concat "\n" (List.map (fun s -> string_of_diag s) n)) );
           ( "" >:: fun _ ->
             let d : diagramme =
               Diag.of_list
                 [
                   (Predicate_set.singleton "a", Vide);
                   (Predicate_set.empty, Vide);
                 ]
             in
             let n = negate_diag d in
             string_eq "{a} -> NonVide\n\n∅ -> NonVide\n"
               (String.concat "\n" (List.map (fun s -> string_of_diag s ) n)) );
         ]
end

module ValiditeViaNegTest = struct
  open ValiditeViaNegation

  let assert_est_valid premisse conclusion est_valid =
    bool_eq est_valid (est_valid_premiss_conc' premisse conclusion)

  let tests =
    ""
    >::: [
           ( "Test 1 (p1 ?⊢ p2 ∨ (p1 ∧ p3)) " >:: fun _ ->
             let premisse = p1 in
             let conclusion = Ou (p2, Et (p1, p3)) in
             assert_est_valid premisse conclusion true );
           ( "Test 2 (p4 ?⊢ p5 ∨ p6)" >:: fun _ ->
             let premisse = p4 in
             let conclusion = Ou (p5, p6) in
             assert_est_valid premisse conclusion false );
         ]
end

let () =
  run_test_tt_main
    ("Global suite"
    >::: [
           ValiditeNaiveTest.tests;
           DiagVennTest.tests;
           ValiditeViaNegTest.tests;
         ])
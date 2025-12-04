
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | TOP
    | PARO
    | PARF
    | OU
    | NON
    | IMP
    | ET
    | EOF
    | BOT
    | ATOME of (
# 6 "Proposition/parser.mly"
       (string)
# 24 "Proposition/parser.ml"
  )
  
end

include MenhirBasics

# 1 "Proposition/parser.mly"
  

# 34 "Proposition/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_f) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: f. *)

  | MenhirState02 : (('s, _menhir_box_f) _menhir_cell1_PARO, _menhir_box_f) _menhir_state
    (** State 02.
        Stack shape : PARO.
        Start symbol: f. *)

  | MenhirState03 : (('s, _menhir_box_f) _menhir_cell1_NON, _menhir_box_f) _menhir_state
    (** State 03.
        Stack shape : NON.
        Start symbol: f. *)

  | MenhirState09 : (('s, _menhir_box_f) _menhir_cell1_form, _menhir_box_f) _menhir_state
    (** State 09.
        Stack shape : form.
        Start symbol: f. *)

  | MenhirState11 : (('s, _menhir_box_f) _menhir_cell1_form, _menhir_box_f) _menhir_state
    (** State 11.
        Stack shape : form.
        Start symbol: f. *)

  | MenhirState13 : (('s, _menhir_box_f) _menhir_cell1_form, _menhir_box_f) _menhir_state
    (** State 13.
        Stack shape : form.
        Start symbol: f. *)


and ('s, 'r) _menhir_cell1_form = 
  | MenhirCell1_form of 's * ('s, 'r) _menhir_state * (Formule.formule)

and ('s, 'r) _menhir_cell1_NON = 
  | MenhirCell1_NON of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PARO = 
  | MenhirCell1_PARO of 's * ('s, 'r) _menhir_state

and _menhir_box_f = 
  | MenhirBox_f of (Formule.formule) [@@unboxed]

let _menhir_action_01 =
  fun e ->
    (
# 24 "Proposition/parser.mly"
                  ( e )
# 85 "Proposition/parser.ml"
     : (Formule.formule))

let _menhir_action_02 =
  fun () ->
    (
# 27 "Proposition/parser.mly"
                                 (Formule.Bot)
# 93 "Proposition/parser.ml"
     : (Formule.formule))

let _menhir_action_03 =
  fun () ->
    (
# 28 "Proposition/parser.mly"
                                 (Top)
# 101 "Proposition/parser.ml"
     : (Formule.formule))

let _menhir_action_04 =
  fun f1 ->
    (
# 29 "Proposition/parser.mly"
                                 (f1)
# 109 "Proposition/parser.ml"
     : (Formule.formule))

let _menhir_action_05 =
  fun x ->
    (
# 30 "Proposition/parser.mly"
                                 (Atome x)
# 117 "Proposition/parser.ml"
     : (Formule.formule))

let _menhir_action_06 =
  fun f1 f2 ->
    (
# 31 "Proposition/parser.mly"
                                 (Ou(f1, f2))
# 125 "Proposition/parser.ml"
     : (Formule.formule))

let _menhir_action_07 =
  fun f1 f2 ->
    (
# 32 "Proposition/parser.mly"
                                 (Et(f1, f2))
# 133 "Proposition/parser.ml"
     : (Formule.formule))

let _menhir_action_08 =
  fun f1 f2 ->
    (
# 33 "Proposition/parser.mly"
                                 (Formule.Imp(f1, f2))
# 141 "Proposition/parser.ml"
     : (Formule.formule))

let _menhir_action_09 =
  fun f1 ->
    (
# 34 "Proposition/parser.mly"
                                 (Formule.Non f1)
# 149 "Proposition/parser.ml"
     : (Formule.formule))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ATOME _ ->
        "ATOME"
    | BOT ->
        "BOT"
    | EOF ->
        "EOF"
    | ET ->
        "ET"
    | IMP ->
        "IMP"
    | NON ->
        "NON"
    | OU ->
        "OU"
    | PARF ->
        "PARF"
    | PARO ->
        "PARO"
    | TOP ->
        "TOP"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_f) _menhir_state -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_03 () in
      _menhir_goto_form _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_form : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_f) _menhir_state -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState00 ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState13 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState11 ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState09 ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState03 ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_15 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_f) _menhir_state -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OU ->
          let _menhir_stack = MenhirCell1_form (_menhir_stack, _menhir_s, _v) in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMP ->
          let _menhir_stack = MenhirCell1_form (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ET ->
          let _menhir_stack = MenhirCell1_form (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF ->
          let e = _v in
          let _v = _menhir_action_01 e in
          MenhirBox_f _v
      | _ ->
          _eRR ()
  
  and _menhir_run_09 : type  ttv_stack. (ttv_stack, _menhir_box_f) _menhir_cell1_form -> _ -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState09 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TOP ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARO ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NON ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOT ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ATOME _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_f) _menhir_state -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PARO (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState02 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TOP ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARO ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NON ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOT ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ATOME _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_f) _menhir_state -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NON (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState03 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TOP ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARO ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NON ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOT ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ATOME _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_f) _menhir_state -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_02 () in
      _menhir_goto_form _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_f) _menhir_state -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_05 x in
      _menhir_goto_form _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_13 : type  ttv_stack. (ttv_stack, _menhir_box_f) _menhir_cell1_form -> _ -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState13 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TOP ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARO ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NON ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOT ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ATOME _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. (ttv_stack, _menhir_box_f) _menhir_cell1_form -> _ -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState11 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TOP ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARO ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NON ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOT ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ATOME _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_14 : type  ttv_stack. ((ttv_stack, _menhir_box_f) _menhir_cell1_form as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_f) _menhir_state -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OU ->
          let _menhir_stack = MenhirCell1_form (_menhir_stack, _menhir_s, _v) in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMP ->
          let _menhir_stack = MenhirCell1_form (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ET ->
          let _menhir_stack = MenhirCell1_form (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | PARF ->
          let MenhirCell1_form (_menhir_stack, _menhir_s, f1) = _menhir_stack in
          let f2 = _v in
          let _v = _menhir_action_08 f1 f2 in
          _menhir_goto_form _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_12 : type  ttv_stack. (ttv_stack, _menhir_box_f) _menhir_cell1_form -> _ -> _ -> _ -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_form (_menhir_stack, _menhir_s, f1) = _menhir_stack in
      let f2 = _v in
      let _v = _menhir_action_07 f1 f2 in
      _menhir_goto_form _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_10 : type  ttv_stack. ((ttv_stack, _menhir_box_f) _menhir_cell1_form as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_f) _menhir_state -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ET ->
          let _menhir_stack = MenhirCell1_form (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EOF | IMP | OU | PARF ->
          let MenhirCell1_form (_menhir_stack, _menhir_s, f1) = _menhir_stack in
          let f2 = _v in
          let _v = _menhir_action_06 f1 f2 in
          _menhir_goto_form _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ((ttv_stack, _menhir_box_f) _menhir_cell1_PARO as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_f) _menhir_state -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PARF ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_PARO (_menhir_stack, _menhir_s) = _menhir_stack in
          let f1 = _v in
          let _v = _menhir_action_04 f1 in
          _menhir_goto_form _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | OU ->
          let _menhir_stack = MenhirCell1_form (_menhir_stack, _menhir_s, _v) in
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IMP ->
          let _menhir_stack = MenhirCell1_form (_menhir_stack, _menhir_s, _v) in
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ET ->
          let _menhir_stack = MenhirCell1_form (_menhir_stack, _menhir_s, _v) in
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. (ttv_stack, _menhir_box_f) _menhir_cell1_NON -> _ -> _ -> _ -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_NON (_menhir_stack, _menhir_s) = _menhir_stack in
      let f1 = _v in
      let _v = _menhir_action_09 f1 in
      _menhir_goto_form _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_f =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState00 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TOP ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PARO ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NON ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | BOT ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ATOME _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
end

let f =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_f v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

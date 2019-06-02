
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | WILDCARD
    | VAR of (
# 5 "parser.mly"
       (Types.var)
# 13 "parser.ml"
  )
    | TRUE
    | TO
    | THEN
    | SUB
    | STR of (
# 4 "parser.mly"
       (string)
# 22 "parser.ml"
  )
    | SPAWN
    | SEND
    | SEMICOLON
    | RPAREN
    | RETURN
    | REF
    | RECV
    | REC
    | RBRACK
    | PICK
    | OR
    | NOT
    | NE
    | MUL
    | MOD
    | MATCH
    | LT
    | LPAREN
    | LET
    | LE
    | LBRACK
    | JOIN
    | INT of (
# 3 "parser.mly"
       (int)
# 49 "parser.ml"
  )
    | IN
    | IF
    | GT
    | GE
    | FUN
    | FALSE
    | EQ
    | EOF
    | END
    | ELSE
    | DIV
    | DEREF
    | CONS
    | COMMA
    | CAT
    | CASE
    | BEGIN
    | AWAIT
    | ASSIGN
    | ARROW
    | AND
    | ADD
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState154
  | MenhirState151
  | MenhirState143
  | MenhirState139
  | MenhirState138
  | MenhirState137
  | MenhirState136
  | MenhirState132
  | MenhirState128
  | MenhirState126
  | MenhirState125
  | MenhirState123
  | MenhirState121
  | MenhirState118
  | MenhirState116
  | MenhirState115
  | MenhirState112
  | MenhirState109
  | MenhirState108
  | MenhirState100
  | MenhirState98
  | MenhirState93
  | MenhirState91
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
  | MenhirState71
  | MenhirState69
  | MenhirState67
  | MenhirState65
  | MenhirState63
  | MenhirState61
  | MenhirState59
  | MenhirState57
  | MenhirState54
  | MenhirState53
  | MenhirState52
  | MenhirState51
  | MenhirState50
  | MenhirState49
  | MenhirState48
  | MenhirState47
  | MenhirState46
  | MenhirState45
  | MenhirState42
  | MenhirState41
  | MenhirState38
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState31
  | MenhirState30
  | MenhirState28
  | MenhirState27
  | MenhirState23
  | MenhirState21
  | MenhirState17
  | MenhirState15
  | MenhirState13
  | MenhirState12
  | MenhirState11
  | MenhirState10
  | MenhirState9
  | MenhirState8
  | MenhirState7
  | MenhirState6
  | MenhirState5
  | MenhirState3
  | MenhirState0

# 51 "parser.mly"
  
  open Types

  (** [fold_fun ps e] is the function [fun p1 -> ... -> fun pn -> e]. *)
  let fold_fun ps e =
    List.fold_right (fun p f -> Fun (p, f)) ps e

  (** [defold_list es] desugars List [e1; ... ; en] to [Cons (e1, ... Cons (en, Nil))] *)
  let rec defold_list es =
    match es with
    | [] -> Nil
    | h::t -> Cons (h, defold_list t)

  (** [defold_plist ps] desugars PList [p1; ... ; pn] to [PCons (p1, ... Cons (pn, Nil))] *)
  let rec defold_plist ps =
    match ps with
    | [] -> PNil
    | p::t -> PCons (p, defold_plist t)


# 186 "parser.ml"

let rec _menhir_goto_nonempty_list_case_ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((Types.pat * Types.exp) list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))), _, (_4 : ((Types.pat * Types.exp) list))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 79 "parser.mly"
                                 ( Match (_2, _4) )
# 208 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Types.pat * Types.exp))), _, (xs : ((Types.pat * Types.exp) list))) = _menhir_stack in
        let _v : ((Types.pat * Types.exp) list) = 
# 223 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( x :: xs )
# 224 "parser.ml"
         in
        _menhir_goto_nonempty_list_case_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run137 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | LBRACK ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | STR _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | TRUE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | VAR _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _v
    | WILDCARD ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137

and _menhir_run57 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState57
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState57 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState57

and _menhir_run69 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState69
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState69 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState69

and _menhir_run71 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState71
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState71 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState71

and _menhir_run73 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState73
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run59 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59

and _menhir_run65 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState65
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState65 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState65

and _menhir_run75 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState75
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run79 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState79
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run83 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState83
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState85
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run67 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67

and _menhir_run89 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState89
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run61 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState61
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState61 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState61

and _menhir_run91 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState91 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91

and _menhir_run87 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState87
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run77 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState77
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_goto_separated_nonempty_list_SEMICOLON_app_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Types.exp list)) = _v in
        let _v : (Types.exp list) = 
# 144 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( x )
# 1219 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_app__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Types.exp list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Types.exp))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Types.exp list) = 
# 243 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( x :: xs )
# 1231 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_app_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | NE | OR | RPAREN | SEMICOLON | SUB | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 134 "parser.mly"
         ( Sub )
# 1363 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1369 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | ASSIGN | CASE | COMMA | CONS | DIV | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | MOD | MUL | NE | OR | RPAREN | SEMICOLON | SUB | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 135 "parser.mly"
         ( Mul )
# 1394 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1400 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
        let _1_inlined1 = () in
        let _v : (Types.exp) = let _2 =
          let _1 = _1_inlined1 in
          
# 146 "parser.mly"
         ( Cat )
# 1419 "parser.ml"
          
        in
        
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1425 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | ASSIGN | CASE | COMMA | CONS | DIV | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | MOD | MUL | NE | OR | RPAREN | SEMICOLON | SUB | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 137 "parser.mly"
         ( Mod )
# 1444 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1450 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | ASSIGN | CASE | COMMA | CONS | DIV | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | MOD | MUL | NE | OR | RPAREN | SEMICOLON | SUB | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 136 "parser.mly"
         ( Div )
# 1475 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1481 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.exp) = 
# 87 "parser.mly"
                                 ( Seq (_1, _3) )
# 1534 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | IN | OR | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 139 "parser.mly"
         ( Or )
# 1583 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1589 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | EQ | IN | NE | OR | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 145 "parser.mly"
         ( Ne )
# 1632 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1638 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | NE | OR | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 140 "parser.mly"
         ( Lt )
# 1673 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1679 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | NE | OR | RPAREN | SEMICOLON | SUB | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 133 "parser.mly"
         ( Add )
# 1710 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1716 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | NE | OR | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 141 "parser.mly"
         ( Le )
# 1751 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1757 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | NE | OR | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 142 "parser.mly"
         ( Gt )
# 1792 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1798 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | NE | OR | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 143 "parser.mly"
         ( Ge )
# 1833 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1839 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | EQ | IN | NE | OR | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 144 "parser.mly"
         ( Eq )
# 1882 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1888 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | AND | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | IN | OR | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _1_inlined1 = () in
            let _v : (Types.exp) = let _2 =
              let _1 = _1_inlined1 in
              
# 138 "parser.mly"
         ( And )
# 1935 "parser.ml"
              
            in
            
# 92 "parser.mly"
                                 ( Bin (_2, _1, _3) )
# 1941 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.exp) = 
# 88 "parser.mly"
                                 ( Cons (_1, _3) )
# 1992 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, (_1 : (Types.exp))), _, (_3 : (Types.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.exp) = 
# 89 "parser.mly"
                                 ( Assign (_1, _3) )
# 2043 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (_2 : (Types.pat))), _), _, (_4 : (Types.exp))), _, (_6 : (Types.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 83 "parser.mly"
                                 ( Await (_2, _4, _6) )
# 2100 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))) = _menhir_stack in
        let _1 = () in
        let _v : (Types.exp) = 
# 91 "parser.mly"
                                 ( Deref _2 )
# 2117 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Types.pat))), _), _, (_4 : (Types.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 80 "parser.mly"
                                 ( Fun (_2, _4) )
# 2167 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState98
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState100
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState100 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState100)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))), _, (_4 : (Types.exp))), _, (_6 : (Types.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 78 "parser.mly"
                                 ( IfThen (_2, _4, _6) )
# 2424 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Types.exp) = 
# 94 "parser.mly"
                                 ( Join (_2) )
# 2475 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 130 "parser.mly"
                                               ( _2 )
# 2511 "parser.ml"
             in
            _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState112
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState112 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState112)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (_2 : (
# 5 "parser.mly"
       (Types.var)
# 2686 "parser.ml"
            ))), _, (_3 : (Types.pat list))), _, (_5 : (Types.exp))), _, (_7 : (Types.exp))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 82 "parser.mly"
                                 ( Let (PVar _2, fold_fun _3 _5, _7) )
# 2694 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState118
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _), (_3 : (
# 5 "parser.mly"
       (Types.var)
# 2847 "parser.ml"
            ))), _), _, (_5 : (Types.exp))), _, (_7 : (Types.exp))) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 97 "parser.mly"
                                 ( LetRec (_3, _5, _7) )
# 2856 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState123
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((((_menhir_stack, _menhir_s), _), (_3 : (
# 5 "parser.mly"
       (Types.var)
# 3009 "parser.ml"
            ))), _, (_4 : (Types.pat list))), _, (_6 : (Types.exp))), _, (_8 : (Types.exp))) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _2 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 98 "parser.mly"
                                 ( LetRec (_3, fold_fun _4 _6, _8) )
# 3018 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState128
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState128 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState128)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((((_menhir_stack, _menhir_s), _, (_2 : (Types.pat))), _), _, (_4 : (Types.exp))), _, (_6 : (Types.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 81 "parser.mly"
                                 ( Let (_2, _4, _6) )
# 3175 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState132
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 127 "parser.mly"
                                               ( _2 )
# 3284 "parser.ml"
             in
            _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))), _, (_4 : (Types.exp))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 128 "parser.mly"
                                               ( Pair (_2, _4) )
# 3343 "parser.ml"
             in
            _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState136
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | CASE | END ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Types.pat))), _), _, (_4 : (Types.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Types.pat * Types.exp) = 
# 114 "parser.mly"
                     ( (_2, _4) )
# 3459 "parser.ml"
             in
            let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | CASE ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) MenhirState143
            | END ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, (x : (Types.pat * Types.exp))) = _menhir_stack in
                let _v : ((Types.pat * Types.exp) list) = 
# 221 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( [ x ] )
# 3474 "parser.ml"
                 in
                _menhir_goto_nonempty_list_case_ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))) = _menhir_stack in
        let _1 = () in
        let _v : (Types.exp) = let _1 = 
# 150 "parser.mly"
        ( Not )
# 3495 "parser.ml"
         in
        
# 93 "parser.mly"
                                 ( Una (_1, _2) )
# 3500 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Types.exp) = 
# 95 "parser.mly"
                                 ( Pick (_2) )
# 3545 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Types.exp) = 
# 85 "parser.mly"
                                 ( Recv (_2) )
# 3596 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))) = _menhir_stack in
        let _1 = () in
        let _v : (Types.exp) = 
# 90 "parser.mly"
                                 ( Ref _2 )
# 3613 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Types.exp) = 
# 96 "parser.mly"
                                 ( Return (_2) )
# 3658 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState151 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))), _, (_4 : (Types.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 84 "parser.mly"
                                 ( Send (_2, _4) )
# 3810 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState154
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | CASE | COMMA | ELSE | END | EOF | IN | RPAREN | SEMICOLON | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))), _, (_4 : (Types.exp))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Types.exp) = 
# 86 "parser.mly"
                                 ( Spawn (_2, _4) )
# 3962 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | ADD | AND | ASSIGN | CASE | COMMA | CONS | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | NE | OR | RPAREN | SEMICOLON | SUB | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (_2 : (Types.exp))) = _menhir_stack in
            let _1 = () in
            let _v : (Types.exp) = let _1 = 
# 149 "parser.mly"
        ( Neg )
# 3991 "parser.ml"
             in
            
# 93 "parser.mly"
                                 ( Una (_1, _2) )
# 3996 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ADD ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | AND ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN ->
            _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
        | CAT ->
            _menhir_run61 _menhir_env (Obj.magic _menhir_stack)
        | CONS ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Types.exp))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 47 "parser.mly"
       (Types.exp)
# 4030 "parser.ml"
            ) = 
# 75 "parser.mly"
          ( _1 )
# 4034 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 47 "parser.mly"
       (Types.exp)
# 4041 "parser.ml"
            )) = _v in
            Obj.magic _1
        | EQ ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GE ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GT ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LE ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LT ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | MOD ->
            _menhir_run65 _menhir_env (Obj.magic _menhir_stack)
        | MUL ->
            _menhir_run59 _menhir_env (Obj.magic _menhir_stack)
        | NE ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | OR ->
            _menhir_run71 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run69 _menhir_env (Obj.magic _menhir_stack)
        | SUB ->
            _menhir_run57 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_pat_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.pat list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (x : (Types.pat))), _, (xs : (Types.pat list))) = _menhir_stack in
        let _v : (Types.pat list) = 
# 223 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( x :: xs )
# 4086 "parser.ml"
         in
        _menhir_goto_nonempty_list_pat_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState41
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState41 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState41)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState121
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_SEMICOLON_pat_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.pat list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Types.pat list)) = _v in
        let _v : (Types.pat list) = 
# 144 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( x )
# 4234 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_pat__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Types.pat list)) = _v in
        let ((_menhir_stack, _menhir_s, (x : (Types.pat))), _) = _menhir_stack in
        let _2 = () in
        let _v : (Types.pat list) = 
# 243 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( x :: xs )
# 4246 "parser.ml"
         in
        _menhir_goto_separated_nonempty_list_SEMICOLON_pat_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run30 : _menhir_env -> 'ttv_tail * _menhir_state * (Types.pat) -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | LBRACK ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | STR _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | TRUE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | VAR _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _v
    | WILDCARD ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_app : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState0 | MenhirState3 | MenhirState5 | MenhirState154 | MenhirState6 | MenhirState151 | MenhirState7 | MenhirState8 | MenhirState9 | MenhirState10 | MenhirState11 | MenhirState12 | MenhirState139 | MenhirState13 | MenhirState132 | MenhirState126 | MenhirState128 | MenhirState121 | MenhirState123 | MenhirState116 | MenhirState118 | MenhirState41 | MenhirState112 | MenhirState45 | MenhirState46 | MenhirState47 | MenhirState98 | MenhirState100 | MenhirState50 | MenhirState51 | MenhirState54 | MenhirState93 | MenhirState69 | MenhirState91 | MenhirState89 | MenhirState71 | MenhirState87 | MenhirState85 | MenhirState73 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState75 | MenhirState77 | MenhirState57 | MenhirState67 | MenhirState65 | MenhirState59 | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | FALSE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | INT _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | LBRACK ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | STR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
        | ADD | AND | ASSIGN | CASE | CAT | COMMA | CONS | DIV | ELSE | END | EOF | EQ | GE | GT | IN | LE | LT | MOD | MUL | NE | OR | RPAREN | SEMICOLON | SUB | THEN | TO | WITH ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Types.exp))) = _menhir_stack in
            let _v : (Types.exp) = 
# 99 "parser.mly"
                                 ( _1 )
# 4315 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63)
    | MenhirState109 | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | BEGIN ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | FALSE ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | INT _v ->
            _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | LBRACK ->
            _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | LPAREN ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState108 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109)
        | STR _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | TRUE ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState108
        | VAR _v ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState108 _v
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Types.exp))) = _menhir_stack in
            let _v : (Types.exp list) = 
# 241 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( [ x ] )
# 4376 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_app_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState108)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_pat__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.pat list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (xs : (Types.pat list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Types.pat) = let _2 = 
# 232 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( xs )
# 4403 "parser.ml"
         in
        
# 111 "parser.mly"
                                                              ( defold_plist _2 )
# 4408 "parser.ml"
         in
        _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_pat : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.pat) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState28 | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState27
        | SEMICOLON ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState27 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | LBRACK ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | LPAREN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | STR _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | TRUE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | VAR _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | WILDCARD ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState28
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | RBRACK ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Types.pat))) = _menhir_stack in
            let _v : (Types.pat list) = 
# 241 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( [ x ] )
# 4462 "parser.ml"
             in
            _menhir_goto_separated_nonempty_list_SEMICOLON_pat_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27)
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31
        | ARROW | COMMA | EQ | FALSE | INT _ | LBRACK | LPAREN | RBRACK | RPAREN | SEMICOLON | STR _ | TRUE | VAR _ | WILDCARD ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, (_1 : (Types.pat))), _), _, (_3 : (Types.pat))) = _menhir_stack in
            let _2 = () in
            let _v : (Types.pat) = 
# 110 "parser.mly"
                                                              ( PCons (_1, _3) )
# 4483 "parser.ml"
             in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState34 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | LBRACK ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | LPAREN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | STR _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | TRUE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | VAR _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
            | WILDCARD ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState35
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
        | CONS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34)
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState36 in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((((_menhir_stack, _menhir_s), _, (_2 : (Types.pat))), _), _, (_4 : (Types.pat))) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : (Types.pat) = 
# 109 "parser.mly"
                                                              ( PPair (_2, _4) )
# 4547 "parser.ml"
             in
            _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36)
    | MenhirState115 | MenhirState38 | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | FALSE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | LBRACK ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | LPAREN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | STR _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | TRUE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | VAR _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
        | WILDCARD ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState38
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Types.pat))) = _menhir_stack in
            let _v : (Types.pat list) = 
# 221 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( [ x ] )
# 4583 "parser.ml"
             in
            _menhir_goto_nonempty_list_pat_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState49 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50)
        | CONS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState49
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState53
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState53 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | CONS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState125
        | EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState125 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState126
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125)
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | ARROW ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_s = MenhirState138 in
            let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | AWAIT ->
                _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | BEGIN ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | DEREF ->
                _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | FALSE ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | FUN ->
                _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | IF ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | INT _v ->
                _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
            | JOIN ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | LBRACK ->
                _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | LET ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | LPAREN ->
                _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | MATCH ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | NOT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | PICK ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | RECV ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | REF ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | RETURN ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | SEND ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | SPAWN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | STR _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
            | SUB ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState139 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139)
        | CONS ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState138
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138)
    | _ ->
        _menhir_fail ()

and _menhir_reduce54 : _menhir_env -> 'ttv_tail * _menhir_state * (
# 5 "parser.mly"
       (Types.var)
# 4868 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s, (_1 : (
# 5 "parser.mly"
       (Types.var)
# 4874 "parser.ml"
    ))) = _menhir_stack in
    let _v : (Types.pat) = 
# 108 "parser.mly"
                                                              ( PVar (_1) )
# 4879 "parser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_loption_separated_nonempty_list_SEMICOLON_app__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.exp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (xs : (Types.exp list))) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : (Types.exp) = let _2 = 
# 232 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( xs )
# 4900 "parser.ml"
         in
        
# 129 "parser.mly"
                                               ( defold_list _2)
# 4905 "parser.ml"
         in
        _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_value : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.exp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 | MenhirState3 | MenhirState154 | MenhirState5 | MenhirState151 | MenhirState6 | MenhirState7 | MenhirState8 | MenhirState9 | MenhirState10 | MenhirState11 | MenhirState139 | MenhirState12 | MenhirState132 | MenhirState13 | MenhirState128 | MenhirState126 | MenhirState123 | MenhirState121 | MenhirState118 | MenhirState116 | MenhirState112 | MenhirState41 | MenhirState109 | MenhirState42 | MenhirState45 | MenhirState46 | MenhirState100 | MenhirState98 | MenhirState47 | MenhirState50 | MenhirState51 | MenhirState93 | MenhirState91 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState71 | MenhirState69 | MenhirState67 | MenhirState65 | MenhirState61 | MenhirState59 | MenhirState57 | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Types.exp)) = _v in
        let _v : (Types.exp) = 
# 118 "parser.mly"
            ( _1 )
# 4925 "parser.ml"
         in
        _menhir_goto_app _menhir_env _menhir_stack _menhir_s _v
    | MenhirState108 | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Types.exp)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Types.exp))) = _menhir_stack in
        let _v : (Types.exp) = 
# 117 "parser.mly"
            ( App (_1, _2) )
# 4936 "parser.ml"
         in
        _menhir_goto_app _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.pat) = 
# 102 "parser.mly"
                                                              ( PWild )
# 4950 "parser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "parser.mly"
       (Types.var)
# 4957 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack)

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.pat) = 
# 104 "parser.mly"
                                                              ( PBool true )
# 4972 "parser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_run20 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "parser.mly"
       (string)
# 4979 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 4 "parser.mly"
       (string)
# 4987 "parser.ml"
    )) = _v in
    let _v : (Types.pat) = 
# 107 "parser.mly"
                                                              ( PStr _1 )
# 4992 "parser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_run21 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | LBRACK ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState21 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Types.pat) = 
# 103 "parser.mly"
                                                              ( PUnit )
# 5021 "parser.ml"
         in
        _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v
    | STR _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | TRUE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | VAR _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | WILDCARD ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21

and _menhir_run23 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | LBRACK ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | STR _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | TRUE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | VAR _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | WILDCARD ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | RBRACK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState23 in
        let _v : (Types.pat list) = 
# 142 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( [] )
# 5065 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_pat__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "parser.mly"
       (int)
# 5076 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 3 "parser.mly"
       (int)
# 5084 "parser.ml"
    )) = _v in
    let _v : (Types.pat) = 
# 106 "parser.mly"
                                                              ( PInt _1 )
# 5089 "parser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_run25 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.pat) = 
# 105 "parser.mly"
                                                              ( PBool false )
# 5101 "parser.ml"
     in
    _menhir_goto_pat _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState154 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState151 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState143 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState139 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState138 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState137 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState136 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState132 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState128 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState126 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState125 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState123 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState121 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState118 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState116 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState115 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState112 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState109 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState108 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState100 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState98 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState93 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState91 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState89 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState87 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState85 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState83 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState81 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState79 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState77 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState75 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState73 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState71 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState69 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState67 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState65 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState63 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState61 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState59 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState57 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState52 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState51 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState50 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState47 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState46 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState45 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState41 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState30 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState23 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState21 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState15 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState13 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState10 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState8 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState7 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 5 "parser.mly"
       (Types.var)
# 5411 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 5 "parser.mly"
       (Types.var)
# 5419 "parser.ml"
    )) = _v in
    let _v : (Types.exp) = 
# 123 "parser.mly"
                                               ( Var _1 )
# 5424 "parser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.exp) = 
# 124 "parser.mly"
                                               ( Bool true )
# 5436 "parser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 4 "parser.mly"
       (string)
# 5500 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 4 "parser.mly"
       (string)
# 5508 "parser.ml"
    )) = _v in
    let _v : (Types.exp) = 
# 122 "parser.mly"
                                               ( Str _1 )
# 5513 "parser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState7
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState7 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState7

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_run9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState9
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState12
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState13 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : (Types.exp) = 
# 126 "parser.mly"
                                               ( Unit )
# 6024 "parser.ml"
         in
        _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | LBRACK ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | REC ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState15 in
        let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | VAR _v ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | EQ ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState115 in
                let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | AWAIT ->
                    _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | BEGIN ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | DEREF ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | FALSE ->
                    _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | FUN ->
                    _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | IF ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | INT _v ->
                    _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | JOIN ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | LBRACK ->
                    _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | LET ->
                    _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | LPAREN ->
                    _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | MATCH ->
                    _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | NOT ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | PICK ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | RECV ->
                    _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | REF ->
                    _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | RETURN ->
                    _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | SEND ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | SPAWN ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | STR _v ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | SUB ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | TRUE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState116
                | VAR _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _v
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116)
            | FALSE ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | INT _v ->
                _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | LBRACK ->
                _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | LPAREN ->
                _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | STR _v ->
                _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | TRUE ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | VAR _v ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState115 _v
            | WILDCARD ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState115
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState115)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | STR _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | TRUE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | VAR _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState15 in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | INT _v ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | LBRACK ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | LPAREN ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | STR _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | TRUE ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | VAR _v ->
            _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v
        | WILDCARD ->
            _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState17
        | CONS | EQ ->
            _menhir_reduce54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17)
    | WILDCARD ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15

and _menhir_run42 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | RBRACK ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState42 in
        let _v : (Types.exp list) = 
# 142 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
    ( [] )
# 6222 "parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_SEMICOLON_app__ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run43 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 3 "parser.mly"
       (int)
# 6290 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 3 "parser.mly"
       (int)
# 6298 "parser.ml"
    )) = _v in
    let _v : (Types.exp) = 
# 121 "parser.mly"
                                               ( Int _1 )
# 6303 "parser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_run48 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LBRACK ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | STR _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | TRUE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | VAR _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | WILDCARD ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Types.exp) = 
# 125 "parser.mly"
                                               ( Bool false )
# 6399 "parser.ml"
     in
    _menhir_goto_value _menhir_env _menhir_stack _menhir_s _v

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState51
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45

and _menhir_run52 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LBRACK ->
        _menhir_run23 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAREN ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | STR _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | TRUE ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | VAR _v ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | WILDCARD ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 47 "parser.mly"
       (Types.exp)
# 6559 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | AWAIT ->
        _menhir_run52 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | BEGIN ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | DEREF ->
        _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUN ->
        _menhir_run48 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run43 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | JOIN ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LBRACK ->
        _menhir_run42 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MATCH ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | PICK ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RECV ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REF ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | RETURN ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SEND ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | SPAWN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STR _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | SUB ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

# 269 "/Users/Keely/.opam/4.06.1/lib/menhir/standard.mly"
  

# 6627 "parser.ml"

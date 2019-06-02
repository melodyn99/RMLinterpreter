open OUnit2
open Types
open Eval

(** [eval_string env s] evaluates the string [s] with the environment
 *  [env].
 *
 * @param env The evaluation enviroment.
 * @param str A string to evaluate.
 * @return The result value of the evaluation.
*)
let eval_string env s =
  let buf = Lexing.from_string s in
  try buf
      |> Parser.program Lexer.token
      |> Eval.eval env
  with
  | Parser.Error ->
    failwith begin
      let s = Lexing.lexeme_start_p buf in
      let e = Lexing.lexeme_end_p buf in
      Printf.sprintf
        "Parse error at %i:%i - %i:%i"
        s.pos_lnum
        (s.pos_cnum - s.pos_bol)
        e.pos_lnum
        (e.pos_cnum - e.pos_bol)
    end

(** [make_u n s] makes an OUnit test named [n] that expects
    [Unit] to evaluate to [VUnit]. *)
let make_u n s =
  [n >:: (fun _ -> assert_equal (VUnit) (eval_string [] s))]

(** [make_i n i s] makes an OUnit test named [n] that expects
    [s] to evalute to [VInt i]. *)
let make_i n i s =
  [n >:: (fun _ -> assert_equal (VInt i) (eval_string [] s))]

(** [make_b n b s] makes an OUnit test named [n] that expects
    [s] to evalute to [VBool b]. *)
let make_b n b s =
  [n >:: (fun _ -> assert_equal (VBool b) (eval_string [] s))]

(** [make_S n s' s] makes an OUnit test named [n] that expects
    [s] to evaluate to [VString s']. *)
let make_s n s' s =
  [n >:: (fun _ -> assert_equal (VStr s') (eval_string [] s))]

(** [make_f f s] makes an OUnit test named [n] that expects
    [Fun (pat, exp)] to evaluate to [VFun (pat, exp, env)]. *)
let make_f n pat exp env s =
  [n >:: (fun _ -> assert_equal (VFun (pat, exp, env)) (eval_string [] s))]

(** [make_p n e1 e2 v1 v2] makes an OUnit test named [n] that expects
    [Pair (e1, e2)] to evaluate to [VPair (v1, v2)]. *)
let make_p n s v1 v2 =
  [n >:: (fun _ -> assert_equal (VPair (v1, v2)) (eval_string [] s))]

(** [make_c e1 e2 v1 v2] makes an OUnit test named [n] that expects
    [Cons (e1, e2)] to evaluate to [VCons v1 v2]. *)
let make_c n s v1 v2 =
  [n >:: (fun _ -> assert_equal (VCons (v1, v2)) (eval_string [] s))]

(** [make_n n] makes an OUnit test named [n] that expects
    [Nil] to evaluate to [VNil]. *)
let make_n n =
  [n >:: (fun _ -> assert_equal (VNil) (eval [] Nil))]

(** [make_r n r v] makes an OUnit test named [n] that expects
    [r] to evaluate to [VRef (v ref)]. *)
let make_r n r v =
  [n >:: (fun _ -> assert_equal (VRef (ref v)) (eval_string [] r))]

(** [make_l n s] makes an OUnit test named [n] that expects
    [s] to evaluate to [expected]. *)
let make_l n s expected =
  [n >:: (fun _ -> assert_equal expected (eval_string [] s))]

(** [make_dr n s] makes an OUnit test named [n] that expects
    [s] to evaluate to [expected]. *)
let make_dr n s expected =
  [n >:: (fun _ -> assert_equal expected (eval_string [] s))]

(** [make_it n s] makes an OUnit test named [n] that expects
    [s] to evaluate to [expected]. *)
let make_it n s expected =
  [n >:: (fun _ -> assert_equal expected (eval_string [] s))]

(** [make_seq n s] makes an OUnit test named [n] that expects
    [s] to evaluate to [(); expected]. *)
let make_seq n s expected =
  [n >:: (fun _ -> assert_equal ((); expected) (eval_string [] s))]

(** [make_lr n s] makes an OUnit test named [n] that expects
    [s] to evaluate to [expected]. *)
let make_lr n s expected =
  [n >:: (fun _ -> assert_equal expected (eval_string [] s))]

(** [make_lr n s] makes an OUnit test named [n] that expects
    [s] to evaluate to [expected]. *)
let make_m n s expected =
  [n >:: (fun _ -> assert_equal expected (eval_string [] s))]

let make_failure n s fail=
  [n >:: (fun _ -> assert_raises (fail) (fun () -> eval_string [] s))]

let suite = "interpreter test suite" >::: List.flatten [
    make_u "unit 1" "()";
    make_u "unit 2" "let x = ref 1 in x := 2";
    make_u "unit 3" "ref 6 := 6";
    make_i "int 1" 15 "5+10";
    make_i "int 2" 32 "6 / 3 * 6 + 20";
    make_i "int 3" 10 "29 % 3 + 5 * 2 - 2";
    make_i "int 4" (-12) "-12";
    make_i "int 5" 3 "7 % 4";
    make_i "int " 4 ("let add x = x + 1 in let double y = 2*y in double (add 1)") ;
    make_b "bool 1" true "3 < 5";
    make_b "bool 2" true "5 <> 2";
    make_b "bool 3" false "\"abc\" ^ \"def\" = \"123\"";
    make_b "bool 4" true "\"abc\" ^ \"def\" <> \"123\"";
    make_b "bool 5" false "\"abc\" ^ \"def\" <> \"abcdef\"";
    make_b "bool 6" true "true && true";
    make_b "bool 7" false "true && false";
    make_b "bool 8" true "true || false";
    make_b "bool 9" false "5 < 3";
    make_b "bool 10" true "5 >= 5";
    make_b "bool 11" false "not true";
    make_b "bool 12" false "3 <= 1";
    make_b "bool 13" true "5 <= 1 || 3 >= 1";
    make_b "bool 14" false "3 < 1 && 1 >= 1";
    make_s "string 1" "abcdef" "\"ab\" ^ \"c\" ^ \"def\"";
    make_s "string 2" "abcabc" "\"abc\" ^ \"abc\"";
    make_f "fun 1" (PWild) (Int 1) [] ("fun _ -> 1");
    make_f "fun 2" (PVar "x") (Int 1) [] ("fun x -> 1");
    make_f "fun 3" (PVar "x") (Bin (Add, Var "x", Int 1)) [] ("fun x -> x + 1");
    make_p "pair 1" "(1+1, 3+5)" (VInt 2) (VInt 8);
    make_p "pair 2" "(\"abc\", \"def\")" (VStr "abc") (VStr "def");
    make_p "pair 3" "(1+2, \"abc\")" (VInt 3) (VStr "abc");
    make_p "pair 4" "(true && false, (4, 5))" (VBool false) (VPair (VInt 4, VInt 5));
    make_l "let 1" "let x = 5 in x" (VInt 5);
    make_l "let 2" "let x = 5 in let y = 3 in x * y" (VInt 15);
    make_l "let 3" "let x = true in let y = false in x && y" (VBool false);
    make_l "let 4" "let x = \"abc\" in let y = \"a\" in x ^ y" (VStr "abca");

    (* make_l "let 5" "let mathfunc x y = (x + y) * y in mathfunc 5 4" (VInt 36); *)

    make_n "nil";
    make_c "cons 1" ("1::[1]") (VInt 1) (VCons (VInt 1, VNil));
    make_c "cons 2" ("1::[2;3]") (VInt 1) (VCons (VInt 2, VCons (VInt 3, VNil)));
    make_c "cons 3" ("1::[]") (VInt 1) (VNil);
    make_c "cons 4" ("\"hi\"::[\"abc\"]") (VStr "hi") (VCons (VStr "abc", VNil));
    make_c "cons 5" ("\"a\"::[\"bc\"; \"ade\"; \"a\"]") (VStr "a") (VCons (VStr "bc", VCons (VStr "ade", VCons (VStr "a", VNil))));
    make_c "cons 6" ("true::[true; false]") (VBool true) (VCons (VBool true, VCons (VBool false, VNil)));
    make_r "ref 1" ("ref 3") (VInt 3);
    make_r "ref 2" ("ref \"abc\"") (VStr "abc");
    make_r "ref 3" ("let x = ref \"abc\" in x") (VStr "abc");
    make_r "ref 4" ("let x = ref 1 in x := 2; x") (VInt 2);
    make_r "ref 5" ("let x = ref true in x") (VBool true);
    make_r "ref 6" ("let x = ref ((true && false) || false) in x") (VBool false);
    make_dr "deref 1" ("!(ref 3)") (VInt 3);
    make_dr "deref 2" ("let x = ref \"ab\" in !x") (VStr "ab");
    make_dr "deref 3" ("let y = ref true in not (!y)") (VBool false);
    make_dr "deref 4" ("let z = ref 5 in (-3 + !z)") (VInt 2);
    make_dr "deref 5" ("let x = 2 in let y = ref x in !y") (VInt 2);
    make_it "ifthen 1" ("if true then false else true") (VBool false);
    make_it "ifthen 2" ("if (5 = 4) then 4 else 3") (VInt 3);
    make_it "ifthen 3" ("if (false = true) then \"abc\" else \"not abc\"") (VStr "not abc");
    make_it "ifthen 4" ("let y = 3 in let x = 2 in if (x <> y) then \"a\" else 3") (VStr "a");
    make_seq "seq 1" ("let x = ref 1 in x := 2; let y = 2 in y") (VInt 2);
    make_seq "seq 2" ("ref 6 := 8; true") (VBool true);
    make_seq "seq 3" ("let x = 1 in let y = ref x in y := 3; ()") (VUnit);
    make_lr "letrec 1" ("let rec sub1 x = if x = 0 then 0 else sub1 (x-1) in sub1 5") (VInt 0);
    make_lr "letrec 2" ("let rec add1 x = if x = 5 then false else add1 (x+1) in add1 0") (VBool false);
    make_lr "letrec 3" ("let rec add x = if x = 5 then x else add (x+1) in add 0") (VInt 5);
    make_lr "letrec 4" ("let rec fac n = if n = 4 then n else fac(n+1)*n in fac 1") (VInt 24);
    make_lr "letrec 5" ("let rec sum n = if n = 10 then n else sum (n+1)+n in sum 0") (VInt 55);
    make_lr "letrec 6" ("let rec fib n = if (n = 1 || n = 0) then 1 else fib(n-1) + fib(n-2) in fib 0") (VInt 1);
    make_lr "letrec 7" ("let rec fib n = if (n = 1 || n = 0) then 1 else fib(n-1) + fib(n-2) in fib 1") (VInt 1);
    make_lr "letrec 6" ("let rec fib n = if (n = 1 || n = 0) then 1 else fib(n-1) + fib(n-2) in fib 2") (VInt 2);
    make_lr "letrec 6" ("let rec fib n = if (n = 1 || n = 0) then 1 else fib(n-1) + fib(n-2) in fib 3") (VInt 3);
    make_lr "letrec 6" ("let rec fib n = if (n = 1 || n = 0) then 1 else fib(n-1) + fib(n-2) in fib 4") (VInt 5);
    make_lr "letrec 6" ("let rec fib n = if (n = 1 || n = 0) then 1 else fib(n-1) + fib(n-2) in fib 5") (VInt 8);
    (*  make_lr "letrec 3" ("let rec sum x acc = if x = 5 then acc else sum (x+1) (acc+x) in sum 0 0") (VInt 10); *)
    make_m "match 1" ("let x = 3 in match x with | 0 -> false | _ -> true end") (VBool true);
    make_m "match 2" ("match true with | [] -> 1 | false -> 2 | true -> 3 end") (VInt 3);
    make_m "match 3" ("match \"pizza\" with |\"\" -> 0 |\"topping\" -> 1 | \"pizza\" -> 2 | _ -> 3 end") (VInt 2);
    make_m "match 4" ("match [] with |[] -> \"hello\" |h::t -> \"bye\" end") (VStr "hello");
    make_m "match 4" ("match [4;5] with |[] -> \"hello\" |h::t -> \"bye\" end") (VStr "bye");
    make_failure "failure 1" ("1 = true") (IncompatibleTypes);
    make_failure "failure 2" ("\"hello\" <> 3") (IncompatibleTypes);
    make_failure "failure 3" ("1 2") (ExpectedFunction);
    make_failure "failure 4" ("1 / 0") (Division_by_zero);
    make_failure "failure 5" ("-true") (ExpectedInt);
    make_failure "failure 6" ("not 5") (ExpectedBool);
    make_failure "failure 7" ("let x = 5 in !x") (ExpectedRef);
    make_failure "failure 8" ("!x") (UnboundVariable);
    make_failure "failure 9" ("match \"hello\" with |[] -> 0 |1 -> 1 end") (InexhaustivePatterns);
    make_failure "failure 10" ("1; 0") (ExpectedUnit);
    make_failure "failure 11" ("1::5") (ExpectedList);
  ]

let _ = run_test_tt_main suite
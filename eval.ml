open Dwt.Infix
open Types

exception ExpectedUnit
exception ExpectedBool
exception ExpectedInt
exception ExpectedFunction
exception ExpectedHandle
exception ExpectedPromise
exception ExpectedRef
exception ExpectedList
exception ExpectedString
exception UnboundVariable
exception InexhaustivePatterns
exception IncompatibleTypes
exception ArgumentMismatch
exception LetMismatch
exception AwaitMismatch

exception Division_by_zero


(** [search_env env var_val] returns the [some_val] bound to [var_val] in the
    given [env]. Raises UnboundVariable if [var_val] is not in [env]. *)
let rec search_env (env: env) (var_val: var) : value =
  match env with
  | [] -> print_endline var_val; raise UnboundVariable
  | (some_var, some_val)::t -> if some_var = var_val
    then some_val
    else search_env t var_val

(** [bin_check op val1 val2] returns a boolean. Raises IncompatibleTypes if
    [val1] and [val2] are different types, or [val1] and [val2] are not
    integers, strings, or booleans. Returns [val1] = [val2] if [op] matches Eq
    and returns [val1] <> [val2] if [op] matches to Ne. *)
let bin_check (op: bin) (val1: value) (val2: value) : bool =
  match op with
  | Eq ->
    begin
      match val1, val2 with
      | VInt (int1), VInt (int2) -> int1 = int2
      | VStr (str1), VStr (str2) -> str1 = str2
      | VBool (bool1), VBool (bool2) -> bool1 = bool2
      | _, _ -> raise IncompatibleTypes
    end
  | Ne ->
    begin
      match val1, val2 with
      | VInt (int1), VInt (int2) -> int1 <> int2
      | VStr (str1), VStr (str2) -> str1 <> str2
      | VBool (bool1), VBool (bool2) -> bool1 <> bool2
      | _, _ -> raise IncompatibleTypes
    end

(** [unwrap env] returns [None] if [env] matches to [None], and a [lst]
    which represents the [env]. *)
let unwrap (env: env option): env =
  match env with
  |None -> []
  |Some (lst) -> lst

(** [match_pat p v] is a recursive helper function that returns [None] where
    [v] does not match [p], and [Some b] where [v] matches [p] producing new
    bindings [b]. *)
let rec match_pat (p: pat) (v:value) : env option =
  match p,v with
  | PUnit, VUnit -> Some []
  | PWild, _ -> Some []
  | PBool (bool_val), VBool (val_bool)->
    if bool_val = val_bool
    then Some []
    else None
  | PInt (int_val), VInt (val_int)->
    if int_val = val_int
    then Some []
    else None
  | PStr (str_val), VStr (val_str)->
    if str_val = val_str
    then Some []
    else None
  | PVar (var_name), some_val -> Some [(var_name, some_val)]
  | PPair (pat1, pat2), VPair (val1, val2) ->
    begin
      let b1 = match_pat pat1 val1 in
      let b2 = match_pat pat2 val2 in
      match b1, b2 with
      | None, _ -> None
      | _, None -> None
      | op1, op2 -> Some ((unwrap op1)@(unwrap op2))
    end
  | PNil, VNil -> Some []
  | PCons (pat1, pat2), VCons (val1, val2) ->
    begin
      let b1 = match_pat pat1 val1 in
      let b2 = match_pat pat2 val2 in
      match b1, b2 with
      | None, _ -> None
      | _, None -> None
      | op1, op2 -> Some ((unwrap op1)@(unwrap op2))
    end
  | _, _ -> None


(** [new_env new_bind old_env] is a recursive function that creates a new
    environment given [new_bind] and and [old_env]. *)
let rec new_env (new_bind: env) (old_env: env)  =
  match new_bind with
  | [] -> old_env
  | (var_name, v)::t -> new_env t ((var_name, v)::(List.remove_assoc var_name old_env))


(* [eval env exp] is [v], where [v] is the result of evaluating [exp] under the
 * environment [env]
 * side effects: produces the applicable i/o side effects *)
let rec eval (env : env) (exp : exp) : value =
  match exp with
  | Unit -> VUnit
  | Bool (bool_val) -> VBool (bool_val)
  | Pair (e1, e2) -> VPair ((eval env e1), (eval env e2))
  | Int (int_val) -> VInt (int_val)
  | Str (str_val) -> VStr (str_val)
  | Var (var_val) -> search_env env var_val
  | Fun (pat_val, exp_val) -> VFun(pat_val, exp_val, env)
  | App (e1, e2) ->
      (try
        let val1 =
        begin
          match eval env e1 with
          | VFun (a,b,c) -> (a,b,c)
          | VFunRec (a,b,c) -> (a, b, !c)
          | _ -> raise ExpectedFunction
        end in
        let val2 = eval env e2 in
        begin
          match val1 with
          | (p_cl, e_cl, env_cl) ->
            let bindings = bind_pat p_cl val2 in
            begin
              match bindings with
              | None -> raise ArgumentMismatch
              | Some x -> let new_envir = new_env x env in
              eval new_envir e_cl
            end
        end
      with UnboundVariable ->
        (begin
          match e1 with
          | Var "print" -> print_string (Pretty.print (eval env e2));
            VUnit
          | Var "println" -> print_endline (Pretty.print (eval env e2));
            VUnit
          | Var "print_detail1" -> print_string (Pretty.print_detail1 (eval env e2));
            VUnit
          | Var "print_detail2" -> print_string (Pretty.print_detail2 (eval env e2));
            VUnit
          | Var "print_detail3" -> print_string (Pretty.print_detail3 (eval env e2));
            VUnit
          | Var "string_of_int" -> VStr (string_of_int (assert_int (eval env e2)))
          (* | VStr "sleep" -> if eval env e2 = VInt then
                      else raise ExpectedHandle *)
          | Var "random" -> VInt (Random.int (assert_int (eval env e2)))
          | Var "ignore" -> ignore (eval env e2); VUnit
          | _ -> raise UnboundVariable
        end))

(*         let val1 =
        begin
          match eval env e1 with
          | VFun (a,b,c) -> (a,b,c)
          | VFunRec (a,b,c) -> (a, b, !c)
          | _ -> raise ExpectedFunction
        end in
      let val2 = eval env e2 in
      begin
        match val1 with
        | (p_cl, e_cl, env_cl) ->
          let bindings = bind_pat p_cl val2 in
          begin
            match bindings with
            | None -> raise ArgumentMismatch
            | Some x -> let new_envir = new_env x env in
              eval new_envir e_cl
          end
      end *)
(*     if (assert_string (eval env e1)) then
      begin
        match (eval env e1) with
        | VStr "print" -> Pretty.print (eval env e2);
          VUnit
        | VStr "println" -> Pretty.print (eval env e2) ^ "\n";
          VUnit
        | VStr "print_detail1" -> Pretty.print_detail1 (eval env e2);
          VUnit
        | VStr"print_detail2" -> Pretty.print_detail2 (eval env e2);
          VUnit
        | VStr "print_detail3" -> Pretty.print_detail3 (eval env e2);
          VUnit
        | VStr "string_of_int" -> VStr (string_of_int (assert_int (eval env e2)))
        (* | VStr "sleep" -> if eval env e2 = VInt then
                    else raise ExpectedHandle *)
        | VStr "random" -> VInt (Random.int (assert_int (eval env e2)))
        | VStr "ignore" -> (eval env e2);
          VUnit
        | _ ->
          let val1 =
            begin
              match eval env e1 with
              | VFun (a,b,c) -> (a,b,c)
              | VFunRec (a,b,c) -> (a, b, !c)
              | _ -> raise ExpectedFunction
            end in
          let val2 = eval env e2 in
          begin
            match val1 with
            | (p_cl, e_cl, env_cl) ->
              let bindings = bind_pat p_cl val2 in
              begin
                match bindings with
                | None -> raise ArgumentMismatch
                | Some x -> let new_envir = new_env x env in
                  eval new_envir e_cl
              end
          end
      end
    else raise ExpectedFunction *)


  | Let (patt, e1, e2) ->
    let val1 = eval env e1 in
    let bindings = bind_pat patt val1 in
    begin
      match bindings with
      | None -> raise LetMismatch
      | Some x -> let new_envir = new_env x env in
        eval new_envir e2
    end
  | LetRec (f, e1, e2) ->
    let (a, b, c) = assert_fun (eval env e1) in
    let env_ref = ref c in
    let new_e = VFunRec (a, b, env_ref) in
    let new_envir = (f, new_e)::env in
    env_ref := new_envir;
    eval !env_ref e2
  | Nil -> VNil
  | Cons (e1, e2) ->
    let val1 = eval env e1 in
    let val2 = assert_list (eval env e2) in
    VCons (val1, val2)
  | Assign (e1, e2) ->
    let loc = eval env e1 |> assert_ref in
    let val2 = eval env e2 in
    loc := val2;
    VUnit
  | Ref (e1) -> VRef (ref (eval env e1))
  | Deref (e1) ->
    begin
      match eval env e1 with
      | VRef (x) -> !x
      | _ -> raise ExpectedRef
    end
  | Bin (operator, e1, e2) ->
    begin
      match operator with
      | Cat -> let str1 = assert_string (eval env e1) in
        let str2 = assert_string (eval env e2) in
        VStr (str1 ^ str2)
      | Add -> let int1 = assert_int (eval env e1) in
        let int2 = assert_int (eval env e2) in
        VInt (int1 + int2)
      | Sub -> let int1 = assert_int (eval env e1) in
        let int2 = assert_int (eval env e2) in
        VInt (int1 - int2)
      | Mul -> let int1 = assert_int (eval env e1) in
        let int2 = assert_int (eval env e2) in
        VInt (int1 * int2)
      | Div -> let int1 = assert_int (eval env e1) in
        let int2 = assert_int (eval env e2) in
        if int2 = 0
        then raise Division_by_zero
        else VInt (int1 / int2)
      | Mod -> let int1 = assert_int (eval env e1) in
        let int2 = assert_int (eval env e2) in
        if int2 = 0
        then raise Division_by_zero
        else VInt (int1 mod int2)
      | Lt -> let int1 = assert_int (eval env e1) in
        let int2 = assert_int (eval env e2) in
        VBool (int1 < int2)
      | Le -> let int1 = assert_int (eval env e1) in
        let int2 = assert_int (eval env e2) in
        VBool (int1 <= int2)
      | Gt -> let int1 = assert_int (eval env e1) in
        let int2 = assert_int (eval env e2) in
        VBool (int1 > int2)
      | Ge -> let int1 = assert_int (eval env e1) in
        let int2 = assert_int (eval env e2) in
        VBool (int1 >= int2)
      | And -> let bool1 = assert_bool (eval env e1) in
        let bool2 = assert_bool (eval env e2) in
        VBool (bool1 && bool2)
      | Or -> let bool1 = assert_bool (eval env e1) in
        let bool2 = assert_bool (eval env e2) in
        VBool (bool1 || bool2)
      | Eq -> VBool (bin_check (Eq) (eval env e1) (eval env e2))
      | Ne -> VBool (bin_check (Ne) (eval env e1) (eval env e2))
    end
  | Una (una_val, expr) ->
    begin
      match una_val, (eval env expr) with
      | Neg, VInt (int_val) -> VInt (-int_val)
      | Neg, _ -> raise ExpectedInt
      | Not, VBool (bool_val) -> VBool (not bool_val)
      | Not, _ -> raise ExpectedBool
    end
  | Seq (e1, e2) ->
    ignore (assert_unit (eval env e1));
    eval env e2
  | IfThen (e1, e2, e3) ->
    let bool_val = assert_bool (eval env e1) in
    if bool_val
    then eval env e2
    else eval env e3
  | Match (e1, pat_list) ->
    let val1 = eval env e1 in
    find_match pat_list val1 env
  | Await (patt, e1, e2) ->
    let prom1 = VDwt (assert_dwt (eval env e1)) in
    let new_env = bind_pat patt prom1 in
    begin
      match new_env with
      |None -> raise AwaitMismatch
      |Some x -> VDwt (assert_dwt (eval (x@env) e2))
    end
  | Spawn (e1, e2) ->
    let f = eval env e1 in
    ignore (assert_fun f);
    let v = eval env e2 in
    VHandle (Dwt.spawn (Serialize.string_of_value f) (Serialize.string_of_value v))
  | Send (e1, e2) ->
    let v1 = eval env e1 in
    let hand = assert_handle (eval env e2) in
    Dwt.send (Serialize.string_of_value v1) hand;
    VUnit
  | Recv (e1) ->
    let hand = assert_handle (eval env e1) in
    VDwt (Dwt.bind (Dwt.recv hand) (fun x -> Dwt.return (Serialize.value_of_string x)))
  | Join (e1) ->
    let lst = assert_list (eval env e1) in
    VDwt (Dwt.bind (Dwt.join (assert_dwt_list lst)) (fun x -> Dwt.return (vlist_of_list x)))
  | Pick (e1) ->
    let lst = assert_list (eval env e1) in
    VDwt (Dwt.pick (assert_dwt_list lst))
  | Return (e1) -> VDwt (Dwt.return (eval env e1))

(** [find_match pat_list v] is a recursive function. Raises InexhaustivePatterns
    if the value does not bind to any pattern, otherwise returns the expression
    associated with the pattern it matches to. *)
and find_match (pat_list: (pat * exp) list) (v: value) (env: env) =
  match pat_list with
  | [] -> raise InexhaustivePatterns
  | (patt, expr)::t ->
    begin
      match bind_pat patt v with
      | None -> find_match t v env
      | Some x -> eval (new_env x env) expr
    end

(** [assert_dwt_list lst] returns a Dwt list. Raises ExpectedList if [lst] is
    not a list and raises ExpectedPromise if [lst] does not contain all
    promises. *)
and assert_dwt_list lst =
  let rec helper some_lst acc =
    match some_lst with
    | VNil -> acc
    | VCons(h, t) -> helper t ((assert_dwt h)::acc)
    | _ -> raise ExpectedList in
  helper lst []

(* [bind_pat p v] is [None] where [v] does not match [p], and [Some b]
 * where [v] matches [p] producing new bindings [b] *)
and bind_pat (p : pat) (v : value) : env option =
  match_pat p v

(* You may use the following utility functions in your implementation.
 * Example usage: [eval env exp |> assert_unit] *)

and assert_unit = function
  | VUnit -> ()
  | v -> raise ExpectedUnit

and assert_bool = function
  | VBool b -> b
  | _ -> raise ExpectedBool

and assert_int = function
  | VInt i -> i
  | _ -> raise ExpectedInt

and assert_fun = function
  | VFun (f, b, env) -> (f, b, env)
  | _ -> raise ExpectedFunction

and assert_handle = function
  | VHandle h -> h
  | _ -> raise ExpectedHandle

and assert_dwt = function
  | VDwt dwt -> dwt
  | _ -> raise ExpectedPromise

and assert_ref = function
  | VRef ref -> ref
  | _ -> raise ExpectedRef

and assert_list vs =
  match vs with
  | VNil | VCons _ -> vs
  | _ -> raise ExpectedList

and assert_string = function
  | VStr s -> s
  | _ -> raise ExpectedString

(* Converts a list into a VList. *)
and vlist_of_list l =
  let rec loop acc = function
    | [] -> acc
    | h::t -> loop (VCons(h,acc)) t in
  loop VNil (List.rev l)

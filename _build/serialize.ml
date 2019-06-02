open Types

(** Serializable values *)
type svalue =
| SUnit
| SBool of bool
| SInt of int
| SStr of string
| SFun of pat * exp * senv
(* When serializing a recursive function, we omit the recursive function from
it's defining environment to prevent stack overflow. (The recursive function
will be added back to its own environment during deserialization. Which is also
why we hold on to the function name.) *)
| SFunRec of var * pat * exp * senv
| SPair of svalue * svalue
| SRef of svalue
| SNil
| SCons of svalue * svalue
| SHandle of int
[@@deriving yojson]

and senv = (var * svalue) list
[@@deriving yojson]

(** [unwrap error opt] is [v] if [opt] is [Some v], or else raises [error]. *)
let unwrap error = function
| Ok v -> v
| Error _ -> failwith error

(** [is_serializable v] is [true] if [v] can be safely
  * sent to another process, or [false] otherwise. *)
let rec is_serializable = function
| VUnit
| VHandle _
| VBool _
| VInt _
| VStr _
| VRef _
| VFun (_, _, _)
| VFunRec _ -> true
| VPair (l, r) -> is_serializable l && is_serializable r
| VNil -> true
| VCons (h, vs) -> is_serializable h && is_serializable vs
| VDwt _ -> false

(* Let [k] be the variable name of the unique recursive function [f] holding [env_ref].
Let [env] be the environment [env_ref] points to. Let [env'] be the same environment
but with the binding (k, f) removed.

[remove_rec env_ref] finds [k] and returns (k, env_ref') where [env_ref'] is a
new reference to [env'].

(For the purpose of serializing a recursive function, we omit the recursive function from
it's defining environment to prevent stack overflow. The recursive function
will be added back to its own environment during deserialization.) *)
let remove_rec env_ref =
  let rec remove_kv env =
    match env with
    | [] -> failwith "Recursion serialization error (see writeup troubleshooting \
    section). Recusive function was not found in its own environemnt."
    | ((k, VFunRec (p, b, env_ref')) as h)::t -> begin
      if env_ref == env_ref' then (k, t)
      else let (k', tail) = remove_kv t in (k', h::tail) end
    | h::t -> begin
      let (k, tail) = (remove_kv t) in (k, h::tail) end in
  remove_kv !env_ref

(** [svalue_of_value v] is a fallible conversion from value [v] to
  * svalue [sv]. Raises an exception if [v] cannot be safely converted. *)
let rec svalue_of_value = function
| VUnit -> SUnit
| VHandle h -> SHandle h
| VBool b -> SBool b
| VInt i -> SInt i
| VStr s -> SStr s
| VRef v -> SRef (svalue_of_value !v)
| VPair (l, r) -> SPair (svalue_of_value l, svalue_of_value r)
| VNil -> SNil
| VCons (v, vs) -> SCons (svalue_of_value v, svalue_of_value vs)
| VFun (v, e, env) -> SFun (v, e, senv_of_env env)
| VFunRec (p, b, env_ref) -> begin
    let var, env' = remove_rec env_ref in
    SFunRec (var, p, b, senv_of_env env') end
| _ -> failwith "Invalid SValue"

and senv_of_env env =
  env |> List.filter (fun (_, v) -> is_serializable v)
      |> List.map (fun (x, v) -> (x, svalue_of_value v))

(** [string_of_value v] is a fallible conversion from value [v] to string.
  * Raises an exception if [v] cannot be serialized.  *)
let string_of_value v =
  v |> svalue_of_value
    |> svalue_to_yojson
    |> Yojson.Safe.to_string

(** [value_of_svalue sv] is an infallible conversion from svalue [sv] to value [v]. *)
let rec value_of_svalue = function
| SUnit -> VUnit
| SHandle h -> VHandle h
| SBool b -> VBool b
| SInt i -> VInt i
| SStr s -> VStr s
| SRef v -> VRef (ref (value_of_svalue v))
| SPair (l, r) -> VPair (value_of_svalue l, value_of_svalue r)
| SNil -> VNil
| SCons (s, ss) -> VCons (value_of_svalue s, value_of_svalue ss)
| SFun (v, e, senv) -> VFun (v, e, env_of_senv senv)
| SFunRec (f, p, b, senv) -> begin
  let env = env_of_senv senv in
  let env_ref = ref env in
  let v = VFunRec (p, b, env_ref) in
  env_ref := (f, v)::env; v
  end

and env_of_senv senv =
  List.map (fun (x, v) -> (x, value_of_svalue v)) senv

(** [value_of_string s] is an infallible conversion from string [s] to value [v]. *)
let value_of_string s =
  s |> Yojson.Safe.from_string
    |> svalue_of_yojson
    |> unwrap "Invalid JSON value"
    |> value_of_svalue

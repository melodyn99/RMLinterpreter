(* A simple REPL. *)



open CamomileLibraryDefault.Camomile
open React
open Lwt
open Lwt.Infix
open LTerm_text

exception Quit

(** Networking constants *)
let self_port = 11000
let server_port = 10020

let usage = Printf.sprintf "%s%s%s%s%s"
  "Welcome to the RML command shell!\n"
  "Type an expression followed by ';;' to evaluate.\n"
  "Use the '#quit;;' command to quit.\n"
  "Use the '#use \"file\";;' command to execute an `RML` file.\n"
  "Use the '#define x e;;' command to define a new variable.\n"

(* +-----------------------------------------------------------------+
   | Utility String Functions                                        |
   +-----------------------------------------------------------------+ *)

(** [startswith str prefix] tests if [str] starts with the string [prefix].
 *  If the length of [prefix] is longer than [str], returns false.
 *  @param str Input string.
 *  @param prefix Prefix to test.
 *  @return Boolean indicating if the string has the given prefix.
 *)
let startswith str prefix : bool = 
  let str_len = String.length str in
  let prefix_len = String.length prefix in
  if str_len < prefix_len then false
  else String.sub str 0 prefix_len = prefix

(** [endswith str suffix] tests if [str] ends with the string [suffix].
 *  If the length of [suffix] is longer than [str], returns false.
 *  @param str Input string.
 *  @param suffix Suffix to test.
 *  @return Boolean indicating if the string has the given suffix.
 *)
let endswith str suffix : bool = 
  let str_len = String.length str in
  let suffix_len = String.length suffix in
  let start = str_len - suffix_len in
  if start < 0 then false
  else String.sub str start suffix_len = suffix

(** [remove_begin str count] removes the first [count] characters of [str].
 *  If the length of [str] is less than [count], returns the empty string.
 *  @param str Input string.
 *  @param count Number of characters to remove from the start.
 *  @return String with specified number of characters removed.
 *)
let remove_begin str count : string = 
  let str_len = String.length str in
  let remaining = str_len - count in
  if remaining <= 0 then ""
  else String.sub str count remaining

(** [remove_end str count] removes the last [count] characters of [str].
 *  If the length of [str] is less than [count], returns the empty string.
 *  @param str Input string.
 *  @param count Number of characters to remove from the end.
 *  @return String with specified number of characters removed.
 *)
let remove_end str count : string = 
  let str_len = String.length str in
  let remaining = str_len - count in
  if remaining <= 0 then ""
  else String.sub str 0 remaining

(* +-----------------------------------------------------------------+
   | Interpreter                                                     |
   +-----------------------------------------------------------------+ *)

(* A simple model of an interpreter. It maintains some state, and exposes a function
 *   eval : state -> input -> (new_state, output) *)
module Interpreter = struct
  type state = { n : int; env : Types.env }
  
  let init_state = {
    n = 0;
    env = [("SELF", Types.VHandle self_port); ("SERVER", Types.VHandle server_port)];
  }
  
  (** Handle the #use command *)
  let rec use state = function
    | [] -> (state, "Syntax: #use \"filename\";;")
    | lst ->
      try
        let raw = lst |> String.concat " " |> String.trim in
        let len = String.length raw in
        if len < 3
        || String.get raw 0 <> '"'
        || String.get raw (len - 1) <> '"'
        then raise (Invalid_argument "");
        let filename = String.sub raw 1 (String.length raw - 2) in
        let ic = open_in filename in
        let len = in_channel_length ic in
        let buf = Bytes.create len in
        let _ = input ic buf 0 len in
        close_in ic;
        eval state (Bytes.to_string buf)
      with
      | Invalid_argument _ -> (state, "Syntax: #use \"filename\";;")
      | Sys_error e -> (state, "Could not open file: " ^ e)

  (** Handle the #define command *)
  and define state x ss =
    let e = String.concat " " ss in
    let v = eval_string state.env e in
    let state' = { state with env = (x, v) :: state.env } in
    (state', x ^ " defined")

  and help state =
    (state, usage)

  (** [handle_command cmd] handles a REPL command.
   *
   * @param cmd The full command line, without the ending ";;".
   * @return The result of the command.
   *)
  and handle_command state cmd : state * string =
    let stripped = remove_begin cmd 1 in
    let ws = Str.regexp "[ \t\n]+" in
    let lst = Str.split ws stripped in
    let (st, out) = match lst with
    | [] -> (state, "Syntax error: empty command")
    | ["exit"]
    | ["quit"] -> raise Quit
    | ["help"] -> help state
    | "use" :: t -> use state t
    | "define" :: x :: e -> define state x e
    | c :: _ -> (state, "Invalid command: \"" ^ cmd ^ "\"\n")
    in
    let new_state = {st with n = st.n + 1} in
    (new_state, out)

  (** [eval_string env str] evaluates the string [str] with the environment
   *  [env].
   *
   * @param env The machine enviroment.
   * @param str The string to evaluate, without the ending ";;".
   * @return The result of the evaluation.
   *)
  and eval_string env str =
    let buf = Lexing.from_string str in
    try buf
        |> Parser.program Lexer.token
        |> Eval.eval env
        |> eval_lwt
    with
    | Parser.Error ->
      failwith begin
        let s = Lexing.lexeme_start_p buf in
        let e = Lexing.lexeme_end_p buf in
        Printf.sprintf
          "Syntax error at %i:%i - %i:%i: %s"
          s.pos_lnum
          (s.pos_cnum - s.pos_bol)
          e.pos_lnum
          (e.pos_cnum - e.pos_bol)
          (String.sub str s.pos_cnum (e.pos_cnum - s.pos_cnum))
      end

  (** Continually schedules [v] on the [Lwt] scheduler until it is completely resolved. *)
  and eval_lwt = function
  | Types.VDwt lwt -> Lwt_main.run (Dwt.danger_danger_if_you_call_this_function_you_will_get_a_zero_on_a8 lwt) |> eval_lwt
  | v -> v

  (** [eval state s] handles the string [s] with the current
   *  state [state].
   *
   * @param state The interpreter state.
   * @param s The string to handle, without the ending ";;".
   * @return A pair of (new state, string result of the command).
   *)
  and eval state s : state * string =
    let trimmed = String.trim s in
    try begin if trimmed = "" then
      (state, "")
    else if startswith trimmed "#" then
      handle_command state trimmed
    else
      let out = s |> eval_string state.env |> Pretty.print_detail1 in
      let new_state = {state with n = state.n + 1} in
      (new_state, out)
    end with
    | e -> (state, string_of_exn e)

  and string_of_exn e =
    let open Eval in
    try raise e with
    | Failure s -> ("\n" ^ s ^ "\n")
    | ExpectedUnit -> "Exception: expected unit"
    | ExpectedBool -> "Exception: expected bool"
    | ExpectedInt -> "Exception: expected int"
    | ExpectedFunction -> "Exception: expected function"
    | ExpectedPromise -> "Exception: expected promise"
    | ExpectedRef -> "Exception: expected reference"
    | ExpectedList -> "Exception: expected list"
    | ExpectedString -> "Exception: expected string"
    | UnboundVariable -> "Exception: uninitialized variable"
    | InexhaustivePatterns -> "Exception: inexhaustive patterns"
    | IncompatibleTypes -> "Exception: comparison operator should be applied to like types"
    | ArgumentMismatch -> "Exception: argument does not match function pattern"
    | LetMismatch -> "Exception: let expression does not match let pattern"
    | AwaitMismatch -> "Exception: await promise value does not match await pattern"
    | _ -> raise e
end

(* +-----------------------------------------------------------------+
   | Prompt and output wrapping                                      |
   +-----------------------------------------------------------------+ *)

(* Create a prompt based on the current interpreter state *)
let make_prompt state =
  let prompt = Printf.sprintf "In  [%d]: " state.Interpreter.n in
  eval [ S prompt ]

(* Format the interpreter output for REPL display *)
let make_output state out =
  let output = Printf.sprintf "Out [%d]: %s" state.Interpreter.n out in
  eval [ S output ]


(* +-----------------------------------------------------------------+
   | Customization of the read-line engine                           |
   +-----------------------------------------------------------------+ *)

(* The history storage *)
let main_history = LTerm_history.create []

(* Initialize and load the history *)
let init_hist () = LTerm_history.load main_history ".replhist"

(* Save the history to file *)
let save_hist () = LTerm_history.save main_history ".replhist"

(* Copied from Utop source code *)
let is_accept : LTerm_read_line.action -> bool = function
  | Accept -> true
  | action -> false
  
(* [read_phrase] is a custom class to read input.
   Input must end with ";;\n".
 *)
class read_phrase ~term ~history ~state = object(self)
  inherit LTerm_read_line.read_line ~history () as super
  inherit [Zed_utf8.t] LTerm_read_line.term term as super_term

  method! show_box = false
  
  method! send_action action =
    let action : LTerm_read_line.action =
      if is_accept action && S.value self#mode <> LTerm_read_line.Edition then
        Accept
      else
        action
    in
    super#send_action action
  
  method! exec = function
    | action :: actions when S.value self#mode = LTerm_read_line.Edition &&
                             is_accept action  -> begin
        Zed_macro.add self#macro action;
        let input = Zed_rope.to_string (Zed_edit.text self#edit) in
        if endswith input ";;" then begin
          (* Input finished. Add to history and remove the ";;". *)
          LTerm_history.add main_history input;
          return (remove_end input 2)
        end else begin
          (* Input not finished, continue. *)
          self#insert (UChar.of_char '\n');
          self#exec actions
        end
      end
    | actions ->
      super_term#exec actions
    
  initializer
    self#set_prompt (S.const (make_prompt state))
end

(* +-----------------------------------------------------------------+
   | Main loop                                                       |
   +-----------------------------------------------------------------+ *)

let rec loop term history state =
  Lwt.catch (fun () ->
    let rl = new read_phrase ~term ~history:(LTerm_history.contents history) ~state in
    rl#run >|= fun command -> Some command)
    (function
      | Sys.Break -> return None
      | exn -> Lwt.fail exn)
  >>= function
  | Some command -> begin
    try
      let state, out = Interpreter.eval state command in
      LTerm.fprintls term (make_output state out)
      >>= fun () ->
      loop term history state
    with
    | Quit -> return ()
    end
  | None ->
    loop term history state

(* +-----------------------------------------------------------------+
   | Entry point                                                     |
   +-----------------------------------------------------------------+ *)

let main server interpreter =
  let for_each _ _ _ = Lwt.return () in
  Io.init self_port server_port;
  Lwt.async (fun () -> Io.bind for_each self_port);
  Unix.create_process
    server
    [| "rml_server.exe";
       interpreter |]
    Unix.stdin
    Unix.stdout
    Unix.stderr |> ignore;
  Lwt_unix.sleep 0.10 >>= fun () ->
  Io.connect server_port >>= fun _ ->
  LTerm_inputrc.load () >>= fun () ->
  init_hist () >>= fun () ->
  Lwt.catch
    begin fun () ->
      let state = Interpreter.init_state in
      Lazy.force LTerm.stdout >>= fun term ->
      LTerm.fprintls term (make_output state usage) >>= fun () ->
      loop term main_history state >>= fun () ->
      save_hist ()
    end
    begin function
      | LTerm_read_line.Interrupt -> save_hist ()
      | exn -> Lwt.fail exn
    end

let () =
  try
    let server = Sys.argv.(1) in
    let interpreter = Sys.argv.(2) in
    Lwt_main.run (main server interpreter)
  with
  | Invalid_argument _ ->
    print_endline "Usage: ./rml_repl.exe <SERVER.EXE> <INTERPRETER.EXE>";
    exit 1

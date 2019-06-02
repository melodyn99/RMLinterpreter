open Lwt.Infix


let server = 10020
let port = ref 10020
let alive = ref 0
let interpreter = ref ""

(** [next_port ()] returns the next port number available for use.
 *  @return An available port number. *)
let next_port () =
  port := !port + 1;
  !port

let rec main file =
  Io.bind ~initialize:(initialize file) ~set_up ~tear_down for_each server

(** Initializes the server.
 *  @param file An optional file - if provided, will spawn process.  *)
and initialize file = fun _ ->
  match file with
  | None -> Lwt.return ()
  | Some file ->
    let port = next_port () in
    alive := !alive + 1;
    Io.spawn_parent
      ~interpreter:!interpreter
      ~server
      ~port
      ~file
    |> Lwt.return

(** Handle a bot connect. *)
and set_up port : unit Lwt.t =
  Lwt_io.printlf "Bot %i connected." port

(** Handler that runs on each accepted connection. *)
and for_each bot ic oc =
  Lwt_io.read_line ic >>= fun message ->
  respond bot message >>= function
  | None -> for_each bot ic oc
  | Some v -> v |> Serialize.string_of_value |> Lwt_io.write_line oc >>= fun () -> for_each bot ic oc

(** Handle a bot disconnect. *)
and tear_down bot : unit Lwt.t =
  Lwt_io.printlf "Bot %i disconnected." bot

(** Respond to a bot message.
 *  @param bot The bot's id.
 *  @param message The serialized message. *)
and respond bot message =
  let open Types in
  begin match Serialize.value_of_string message with
  | VPair (VStr "echo", v) -> Lwt.return (Some v)
  | VPair (VStr "spawn", (VPair (f, a))) ->
    let port = next_port () in
    let hand = VHandle port in
    alive := !alive + 1;
    Io.spawn_child
      ~interpreter:!interpreter
      ~server
      ~parent:bot
      ~child:port
      ~f:(Serialize.string_of_value f)
      ~a:(Serialize.string_of_value a);
    Lwt.return (Some hand)
  | _ -> Lwt.fail_with ("Invalid message " ^ message)
  end

(** Sets up SIGCHLD handlers. *) 
let handle_sigchild () =
  Sys.set_signal
    (Sys.sigchld)
    (Sys.Signal_handle (fun _ -> if !alive = 1 then exit 0 else alive := !alive - 1))

let () =
  try
    interpreter := Sys.argv.(1); 
    if Array.length Sys.argv > 2 then begin
      handle_sigchild ();
      Lwt_main.run (main (Some Sys.argv.(2)))
    end else
      Lwt_main.run (main None)
  with
  | _ -> print_endline "Usage: ./rml_server.exe <INTERPRETER> <FILE>"

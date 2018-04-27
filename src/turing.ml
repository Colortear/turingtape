(* Files as modules: Machine, State *)

(* 1. validate args
 * 2. parse json file into machine object (object holds state type)
 * 3. set initial state, sending machine object through monad return
 * 4. run through transition loop until the return value isnt 1:
 *        1  = keep running
 *        0  = finished successfully
 *        -1 = the state transition was blocked
 *              (potentially check for infinite loops.
 *                  if state doesnt change in a step use this return
 * 5. if the args are invalid then output usage, if machine is built
 *      incorrectly print_error
*)

open Machine
open State

let usage =
  ("usage: "^Sys.argv.(0)^" [-h] jsonfile input")

let help =
  "\npositional arguments:
    jsonfile                json description of machine

    input                   input of machine

optional arguments:
    -h, --help              show this help message and exit"

let exit_status code =
  match code with
  | -2 -> print_endline "Machine stopped: operation could not be found."
  | -1 -> print_endline "Machine stopped: end state was impossible."
  | 0 -> print_endline "\027[32m\nTERMINATED SUCCESS\n\027[0m"
  | _ -> print_endline "this should never happen"

let args_valid args =
  let len = Array.length args in
  if len > 1 &&
     (String.compare args.(1) "-h" = 0 || 
      String.compare args.(1) "--help" = 0) then 2
  else if len = 3 then 1
  else -1

let print_error err =
  begin
    print_endline usage;
    if err = 2 then
      print_endline help;
  end

let main_loop() =
  let js = try Yojson.Basic.from_file Sys.argv.(1)
    with
      Yojson.Json_error(_) -> `Null
  in
  let module M = MachineMake(struct let j = js end) in
  let str_len = String.length Sys.argv.(2) in
  if M.validate_json_print () = 1 then
    let init_str =
      if str_len < 16 then
        (Sys.argv.(2)^(String.make (16 - str_len) M.blank.[0]))
      else Sys.argv.(2)
    in
    (* transition serves as the core loop of the program *)
    let rec transition state =
      let status = (State.verify_state state M.finals) in
      if status > -3 && status < 1 then
        exit_status status
      else
        transition (State.next state M.trans)
    in
    transition (State.create M.initial init_str M.blank)

let () =
  match args_valid Sys.argv with
  | 1 -> main_loop()
  | x -> print_error x

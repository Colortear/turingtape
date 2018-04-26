open Machine
(* Files as modules: Utils, Machine *)

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
  | -1 -> print_endline "Machine stopped: end state was impossible."
  | 1 -> print_endline "End state reached."
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
  print_endline M.name
(*  let rec transition state_ok state =
    if state_ok = -1 || state_ok = 0 then
      exit_status state_ok
    else
      let new_state = State.next state M.trans in
      transition (State.verify_state new_state) new_state
    in
    transition 1 (State.create M.trans M.initial Sys.argv.(2))*)

let () =
  match args_valid Sys.argv with
  | 1 -> main_loop()
  | x -> print_error x

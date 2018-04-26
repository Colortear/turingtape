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

let exit_status code =
  match code with
  | -1 - > print_endline "Machine stopped: end state was impossible."
  | 1 -> print_endline "End state reached."

let args_valid args =
  if Array.length args <> 2 then -1
  else if String.compare Sys.argv.(1) "-h" = 0 then 2
  else 1

let print_error err =
  begin
    print_usage();
    if err = 2 then
      print_help();
  end

let main_loop machine =
  let module M = (val machine : MACHINE) in
  let rec transition state_ok state =
    if state_ok = -1 || state_ok = 0 then
      exit_status state_ok
    else
      let new_state = State.next state M.trans in
      State.transition (State.verify_state new_state) new_state
  in
  transition 1 (State.create M.trans)

let () =
  match args_valid Sys.argv with
  | 1 -> main_loop (module Machine.MakeMachine(Machine.Dump))
  | _ -> print_error arg_ok

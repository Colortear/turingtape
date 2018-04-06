open Monads
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

let exit_error() =
  print_endline "Machine stopped: end state was impossible."

let exit_success() =
  print_endline "End state reached."

let () =
  let arg_ok = Utils.args_valid Sys.argv in
  if arg_ok = 1 then
    begin
      let initial_state = State.return machine in
      let rec transition state_ok state =
        if state_ok = -1 then
          exit_error()
        else if state_ok = 0 then
          exit_success()
        else
          let new_state = State.bind state (State.next machine) in
          transition (State.verify_state state new_state = 1) new_state
      in
      transition 1 initial_state
    end
  else
    begin
      print_usage();
      if arg_ok = 2 then
        print_help();
    end

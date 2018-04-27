open Yojson.Basic.Util
open Yojson.Basic

type dir = Nil | Left | Right

(*(read * to_state * write * direction) list*)

type s =
  | N
  | Trans of (string, (string * string * string * dir) list) Hashtbl.t

module type JSON =
sig
  val j : Yojson.Basic.json
end

module type MACHINE = functor (Dump : JSON) ->
sig
  val json_dump : Yojson.Basic.json
  val name : string
  val alphabet : string list
  val blank : string
  val states : string list
  val initial : string
  val finals : string list
  val trans : s
  val validate_json_print : unit -> int
end

let member_to_string js name =
  let mem = try member name js
    with Type_error(_,js) -> `Null
  in
  match to_string_option mem with
  | None -> ""
  | Some x -> x

let member_to_list js name =
  let mem = try member name js 
    with Type_error(_,js) -> `Null
  in
  try to_list mem with
    Type_error(_,mem) -> []

let member_to_string_list js name =
  let l = member_to_list js name in
  let rec aux ll =
    match ll with
    | [] -> []
    | hd::tl ->
      match to_string_option hd with
      | None -> [""]
      | Some x -> x::(aux tl)
  in
  aux l

module MachineMake : MACHINE = functor (Dump : JSON) ->
struct
  let json_dump = Dump.j
  let name = member_to_string json_dump "name"
  let alphabet = member_to_string_list json_dump "alphabet"
  let blank = member_to_string json_dump "blank"
  let states = member_to_string_list json_dump "states"
  let initial = member_to_string json_dump "initial"
  let finals = member_to_string_list json_dump "final"
  let trans =
    if json_dump = `Null then N
    else
      let tbl = Hashtbl.create 1720 in
      let trans_obj = json_dump |> member "transitions" in
      let trans_list = trans_obj |> to_assoc in
      let rec aux table l =
        match l with
        | [] -> table
        | hd::tl ->
          let rec loop ll =
            match ll with
            | [] -> []
            | hd2::tl2 ->
              let action =
                begin
                  if String.compare (member_to_string hd2 "action") "LEFT" = 0
                  then Left
                  else if String.compare (member_to_string hd2 "action") "RIGHT" = 0
                  then Right
                  else Nil
                end
              in
              ((member_to_string hd2 "read"),
               (member_to_string hd2 "to_state"),
               (member_to_string hd2 "write"),
               (action))::(loop tl2)
          in
          let (str,jobj) = hd in
          Hashtbl.add table str (loop(member_to_list trans_obj str));
          aux table tl
      in
      Trans(aux tbl trans_list)

  let validate_json_print () =
    if (String.length name = 0) ||
       alphabet = [] || (String.length blank != 1) ||
       states = [] || (String.length initial = 0) ||
       finals = [] (*|| trans = Nil*) then -1
    else 1 (*needs so many error cases added to this omg lmao fuc *)
      (*print the json values if successful, fail with -1 otherwise*)
end

(*move the below code to a debug file for unit testing
 *
let print_list l =
  let rec aux ll =
    match ll with
    | [] -> print_endline ""
    | hd::tl ->
      begin
        print_endline hd;
        aux tl
      end
  in
  aux l

let print_from_table h tt =
  let rec loop y =
    match y with
    | [] -> print_endline ""
    | hd::tl ->
      let (read, next_state, write, action) = hd in
      begin
        print_endline ("read: "^read);
        print_endline ("next_state: "^next_state);
        print_endline ("write: "^write);
        print_endline "action: ";
        if action = Left then print_endline "LEFT"
        else if action = Right then print_endline "RIGHT"
        else print_endline "NULL";
        print_endline "";
        loop tl
      end
  in
  begin
    print_endline (h^":");
    try loop (Hashtbl.find tt h)
    with Not_found -> print_endline "no_listing"
  end

let print_trans t s =
  match t with
  | N -> print_endline "NULL"
  | Trans(tt) ->
    let rec aux ss =
      match ss with
      | [] -> print_endline ""
      | hd::tl ->
        begin
          print_from_table hd tt;
          aux tl
        end
    in
    aux s


let () =
  if Machine.validate_json() = 1 then print_endline "Validate Success!"
  else print_endline "Validate Failure";
  print_endline Machine.name;
  print_list Machine.alphabet;
  print_endline Machine.blank;
  print_list Machine.states;
  print_endline Machine.initial;
  print_list Machine.finals;
  print_trans Machine.trans Machine.states;
  print_endline "...........................testing finished";*)

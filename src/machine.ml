(*
 *   This file is gross pls save yourself and don't try reading mucch of it.
 *   It contains many impurities.
 *)

(* need to handle exceptions in the utilty istring function calls *)

type dir = Nil | Left | Right

type s =
      | Null
      | Trans of (string, (string * string * string * dir) list) Hashtbl.t


open Yojson.Basic.Util
open Yojson.Basic

module type JSON =
  sig
    val j : Yojson.Basic.json
  end

module Dump : JSON =
  struct
    (*change to: *)
(*    let json_dump = Yojson.Basic.from_file Sys.argv.(1)*)
    let j = Yojson.Basic.from_file "../machines/unary_sub.json"
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
    val validate_json : unit -> int
  end

let member_to_string json name =
  match json |> member name |> to_string_option with
  | None -> ""
  | Some x -> x

let member_to_string_list json name =
  json |> member name |> to_list |> filter_string

let member_to_list json name =
  json |> member name |> to_list
 
module MachineMake : MACHINE = functor (Dump : JSON) ->
  struct
    let json_dump = Dump.j
    let name = member_to_string json_dump "name"
    let alphabet = member_to_string_list json_dump "alphabet"
    let blank = member_to_string json_dump "blank"
    let states = member_to_string_list json_dump "states"
    let initial = member_to_string json_dump "initial"
    let finals = member_to_string_list json_dump "finals"
    let trans =
      let tbl = Hashtbl.create 1720 in
      let trans_list = json_dump |> member "transitions" |> to_assoc in
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
          Hashtbl.add table str (loop(member_to_list jobj str));
          aux table tl
      in
      Trans(aux tbl trans_list)

    let validate_json () =
      if (*json_dump = Null ||*) (String.length name = 0) ||
         alphabet = [] || (String.length blank != 1)
         || states = [] || (String.length initial = 0) ||
         finals = [] (*|| trans = Nil*) then -1
      else 1 (*needs so many error cases added to this omg lmao fuc *)
  end

module Machine = MachineMake(Dump)

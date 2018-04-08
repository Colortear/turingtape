open Core.Std
open Yojson.Basic.Util

module type MACHINE =
  sig
    type dir = Left | Right
    type s =
      | Nil
      | Trans of string * ((string * string * string * dir) list) list
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
  match json |> member name |> to_string with
  | Type_error -> ""
  | x -> x

let member_to_list json name =
  json |> member name |> to_list |> filter_string
 
module MachineMake : MACHINE = functor () ->
  struct
    type dir = Left | Right
    type s =
      | Nil
      | Trans of
          (string * ((string * string * string * dir) list) Hashtble.t
     (* | Trans of ( string * ((string * string * string * dir) list )) list*)
    let json_dump = Yojson.Basic.from_file Sys.argv.(1)
    let name =
      match json_dump |> member "name" |> to_string with
      | Type_error -> ""
      | x -> x
    let alphabet = json_dump |> member "alphabet" |>
                   to_list |> filter_string
    let blank =
      match json_dump |> member "blank" |> to_string with
      | Type_error -> ""
      | x -> x
    let states = json_dump |> member "states" |>
                 to_list |> filter_string
    let initial =
      match json_dump |> member "initial" |> to_string with
      | Type_error -> ""
      | x -> x
    let finals = json_dump |> member "finals" |>
                 to_list |> filter_string
    let trans =
      let tbl = Hashtbl.create 1720 in
      let transitions = json |> member "transitions" |> to_list in
      let rec aux table =
        match transitions with
        | [] -> table
        | hd::tl ->
          let hd = hd |> member 
          Hashtbl.add (aux table
      in
      let ret = aux tbl in
      if ret = Null then Nil
      else Trans(ret)
    let validate_json () =
      if json_dump = null || (String.length name = 0) ||
         alphabet = [] || (String.length blank != 1)
         || states = [] || (String.length initial = 0) ||
         finals = [] || trans = Nil then -1
      else 1
  end

module Machine : MACHINE = MachineMake()

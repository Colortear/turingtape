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
 
module MachineMake : MACHINE = functor () ->
  struct
    type dir = Left | Right
    type s =
      | Nil
      | Trans of string * ((string * string * string * dir) list ) list
    let json_dump = Yojson.Basic.from_file Sys.argv.(1)
    let name =
      match json_dump |> member "name" |> to_string with
      | Type_error -> ""
    let alphabet = json_dump |> member "alphabet" |>
                   to_list |> filter_string
    let blank =
      match json_dump |> member "blank" |> to_string with
      | Type_error -> ""
    let states = json_dump |> member "states" |>
                 to_list |> filter_string
    let initial =
      match json_dump |> member "initial" |> to_string with
      | Type_error -> ""
    let finals = json_dump |> member "finals" |>
                 to_list |> filter_string
    let trans =
      let rec aux =
    let validate_json () =
      if json_dump = null || (String.length name = 0) ||
         alphabet = [] || (String.length blank != 1)
         || states = [] || (String.length initial = 0) ||
         finals = [] || trans = Nil then -1
      else 1
  end

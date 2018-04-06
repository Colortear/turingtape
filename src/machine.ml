open Core.Std

module type MACHINE =
  sig
    type dir = Left | Right
    val json_dump : json
    val name : string
    val alphabet : string list
    val blank : string
    val states : string list
    val initial : string
    val finals : string list
    val trans : (string * string * string * dir) list
  end
 
module MachineMake : MACHINE =
  struct
    type dir = Left | Right
    let json_dump = 
    let name = 

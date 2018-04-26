open Machine

module type STATE =
  sig
    (* S of state_string * current_pos * current_transition *)
    type t = Nilt | S of string * int * string

    val create : string -> string -> t
    val next : t -> Machine.s -> t
    val verify_state : t -> int
  end

module State : STATE =
  struct
    let create init input_string =
        S(input_string,0,init)
    let next state trans =
      (*some more complicated state stuff*);
    let verify_state state =
      (*check the state of things *);
  end

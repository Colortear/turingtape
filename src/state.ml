open Machine

(* tape * cur_index * cur_trans * range_start * blank_char *)
type t = Nilt | S of string * int * string * int * string

let create init input_string blank = S(input_string,0,init,0,blank)

let hashtbl_get tbl input =
  try Hashtbl.find tbl input with
    Not_found -> []

let print_state state trans =
  let (read,to_state,write,dir) = trans in
  match state with
  | Nilt -> print_endline "Something went wrong."
  | S(tape,idx,cur_t,view,blank) ->
    let lhs = if idx > view then
        String.sub tape view idx
      else "" in
    let rhs = if idx < view + 15 then
        String.sub tape (idx + 1) ((view + 15) - idx)
      else "" in
    let printable_tape =
      lhs^"<"^(String.make 1 tape.[idx])^">"^rhs^" "
    in
    let print_dir =
      match dir with
      | Left -> "LEFT"
      | Right -> "RIGHT"
      | Nil -> ""
    in
    let operation =
      ("("^cur_t^", "^read^") -> ("^to_state^", "^write^", "^print_dir^")") in
    print_endline (printable_tape^operation)

let next state trans =
  match state with
  | Nilt -> Nilt
  | S(tape,index,t,view,blank) ->
    match trans with
    | N -> Nilt
    | Trans(tbl) ->
      let l = hashtbl_get tbl t in
      let move_head tape' dir' index' ts' view' =
        let ret_tape =
          if index' < 0 then (blank^tape')
          else if index' = String.length tape' then (tape'^blank)
          else tape'
        in
        let ret_idx = if index' < 0 then 0 else index' in
        let ret_view =
          if ret_idx < view' then (view' - 1)
          else if ret_idx > view' + 15 then (view' + 1)
          else view'
        in
        S(ret_tape,ret_idx,ts',ret_view,blank)
      in
      let transition cur_trans =
        let (_, ts, w, d) = cur_trans in
        let new_index =
          if d = Left then (index - 1)
          else if d = Right then (index + 1)
          else index
        in
        begin
          print_state state cur_trans;
          Bytes.set tape index w.[0];
          move_head tape d new_index ts view
        end
      in
      let rec aux ll =
        match ll with
        | [] -> Nilt
        | hd::tl ->
          let (read, to_state, write, dir) = hd in
          if read.[0] = tape.[index] then
            transition hd
          else
            aux tl 
      in
      aux l

let verify_state state finals =
  match state with
  | Nilt -> -1
  | S(tape,idx,cur_t,view,blank) ->
    let rec aux ll =
      match ll with
      | [] -> 1
      | hd::tl ->
        if String.compare hd cur_t = 0 then 0
        else aux tl
    in
    aux finals

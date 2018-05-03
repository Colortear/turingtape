let trim ?blank:(b="") s =
  let trimmings = (" \t\n"^b) in
  let trim_len = (String.length trimmings) - 1 in
  let rec is_trimming idx trim_idx =
    if trim_idx < 0 then false
    else if s.[idx] = trimmings.[trim_idx] then true
    else is_trimming idx (trim_idx - 1)
  in
  let move_idx idx is_left is_moving =
    if is_moving = true then
      begin
        if is_left = true then (idx+1)
        else (idx-1)
      end
    else idx
  in
  let rec aux start length =
    if start > length then ""
    else
      let left_idx =
        move_idx start true (is_trimming start trim_len) in
      let right_idx =
        move_idx length false (is_trimming length trim_len) in
      if left_idx = start && right_idx = length then
        String.sub s left_idx ((right_idx + 1) - left_idx)
      else
        aux left_idx right_idx
  in
  aux 0 ((String.length s) - 1)

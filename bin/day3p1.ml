(* this is my first functor ever *)
module MyMap = Map.Make (Char)

let get_compartments () =
  try
    let line = read_line () in
    let line_len = String.length line in
    let half_len = line_len / 2 in
    let first_half = String.sub line 0 half_len in
    let second_half = String.sub line half_len (line_len - half_len) in
    first_half, second_half
  with
  | End_of_file -> "", ""
;;

let map_create str =
  let map = MyMap.empty in
  let map_chars =
    String.fold_left (fun acc char -> MyMap.add char 0 acc) map str
  in
  map_chars
;;

let lowercase_index c =
  let ascii_value = Char.code c in
  let base_value = Char.code 'a' in
  ascii_value - base_value + 1
;;

let uppercase_index c =
  let ascii_value = Char.code c in
  let base_value = Char.code 'A' in
  ascii_value - base_value + 1 + 26
;;

let get_alphabet_index c =
  if 'a' <= c && c <= 'z'
  then lowercase_index c
  else if 'A' <= c && c <= 'Z'
  then uppercase_index c
  else 0 (* Not an alphabet character *)
;;

let rec run s =
  let first, second = get_compartments () in
  match first, second with
  | "", "" -> s
  | _, _ ->
    let first_map = map_create first in
    let result = ref None in
    String.iter
      (fun c ->
        if MyMap.mem c first_map && !result = None then result := Some c)
      second;
    (match !result with
     | Some r ->
       run s + (get_alphabet_index r)
     | None ->
       run s)
;;

let () =
    let s = 0 in
    let r = run s in
    Printf.printf "%d\n" r

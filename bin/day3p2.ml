module MyMap = Map.Make (Char)

let rec get_three list =
  try
    let line = read_line () in
    match List.length list with
    | 2 -> List.rev (line :: list)
    | _ -> get_three (line :: list)
  with
  | End_of_file -> List.rev list
;;

let get_common list =
  let map_first_c =
    List.nth list 0
    |> String.fold_left (fun acc char -> MyMap.add char 0 acc) @@ MyMap.empty
  in
  let map_second_c =
    List.nth list 1
    |> String.fold_left (fun acc char ->
         if MyMap.mem char map_first_c then MyMap.add char 0 acc else acc)
       @@ MyMap.empty
  in
  let result = ref None in
  List.nth list 2
  |> String.iter (fun c ->
       if MyMap.mem c map_second_c && !result = None then result := Some c);
  !result
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
  let three = get_three [] in
  match three with
  | [] -> s
  | _ ->
    let common = get_common three in
    (match common with
     | Some c ->
       let num = get_alphabet_index c in
       run s + num
     | None -> s)
;;

let () =
  let s = 0 in
  let r = run s in
  Printf.printf "%d\n" r
;;

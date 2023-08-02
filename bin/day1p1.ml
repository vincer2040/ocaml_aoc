open Printf

let rec get_cur_list list =
  try
    let line = read_line () in
    match line with
    | "" -> list
    | _ ->
      let at_int = int_of_string line in
      get_cur_list (at_int :: list)
  with
  | End_of_file -> list
;;

let sum_int_list list = List.fold_left (fun acc x -> acc + x) 0 list

let rec run top =
  let list = get_cur_list [] in
  match list with
  | [] -> top
  | _ ->
    let sum = sum_int_list list in
    if sum > top then run sum else run top
;;

(*let print_int_list list = List.iter (fun x -> printf "%d\n" x) list*)

let () =
  let s = 0 in
  let top = run s in
  printf "top: %d\n" top
;;


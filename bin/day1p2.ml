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

let rec run (a, b, c) =
  let list = get_cur_list [] in
  match list with
  | [] -> a, b, c
  | _ ->
    let sum = sum_int_list list in
    if sum > a
    then run (sum, a, b)
    else if sum > b
    then run (a, sum, b)
    else if sum > c
    then run (a, b, sum)
    else run (a, b, c)
;;

(*let print_int_list list = List.iter (fun x -> printf "%d\n" x) list*)

let () =
  let a, b, c = run (0, 0, 0) in
  let list = [ a; b; c ] in
  let sum = sum_int_list list in
  printf "%d\n" sum
;;


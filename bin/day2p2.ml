type play =
  | Done
  | Rock
  | Paper
  | Scissors

type outcome =
  | Done
  | Win
  | Draw
  | Lost

let get_oppenent_play str =
  match str with
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _ -> failwith "invalid oppenent play"
;;

let get_outcome str =
  match str with
  | "X" -> Lost
  | "Y" -> Draw
  | "Z" -> Win
  | _ -> failwith "invalid me play"
;;

let get_plays game =
  let opp_play = List.nth game 0 in
  let outcome = List.nth game 1 in
  let opp = get_oppenent_play opp_play in
  let me = get_outcome outcome in
  opp, me
;;

let get_game () =
  try
    let line = read_line () in
    let game = String.split_on_char ' ' line in
    let plays = get_plays game in
    plays
  with
  | End_of_file -> Done, Done
;;

let get_my_play game =
  match game with
  | Rock, Win -> Paper
  | Rock, Lost -> Scissors
  | Rock, Draw -> Rock
  | Paper, Win -> Scissors
  | Paper, Lost -> Rock
  | Paper, Draw -> Paper
  | Scissors, Win -> Rock
  | Scissors, Lost -> Paper
  | Scissors, Draw -> Scissors
  | _ -> failwith "invalid game"
;;

let get_outcome_score outcome =
  match outcome with
  | Done -> 0
  | Win -> 6
  | Draw -> 3
  | Lost -> 0
;;

let play_score play =
  match play with
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3
  | Done -> 0
;;

let rec run s =
  let opp, outcome = get_game () in
  match opp, outcome with
  | Done, Done -> s
  | _ ->
    let outcome_score = get_outcome_score outcome in
    let my_play = get_my_play (opp, outcome) in
    let ps = play_score my_play in
    run s + outcome_score + ps
;;

let () =
  let s = 0 in
  let score = run s in
  Printf.printf "%d\n" score
;;

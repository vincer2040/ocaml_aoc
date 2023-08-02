type play =
  | Rock
  | Paper
  | Scissors
  | Done

type outcome =
  | Win
  | Draw
  | Lost
  | End

let get_oppenent_play str =
  match str with
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _ -> failwith "invalid oppenent play"
;;

let get_my_play str =
  match str with
  | "X" -> Rock
  | "Y" -> Paper
  | "Z" -> Scissors
  | _ -> failwith "invalid me play"
;;

let get_plays game =
  let opponent = List.nth game 0 in
  let me = List.nth game 1 in
  let oppenent_play = get_oppenent_play opponent in
  let my_play = get_my_play me in
  oppenent_play, my_play
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

let get_outcome game =
  match game with
  | Done, Done -> End
  | Rock, Rock -> Draw
  | Rock, Paper -> Win
  | Rock, Scissors -> Lost
  | Paper, Rock -> Lost
  | Paper, Scissors -> Win
  | Paper, Paper -> Draw
  | Scissors, Rock -> Win
  | Scissors, Paper -> Lost
  | Scissors, Scissors -> Draw
  | _ -> failwith "invalid game"
;;

let outcome_score outcome =
  match outcome with
  | End -> 0
  | Win -> 6
  | Draw -> 3
  | Lost -> 0
;;

let play_score play =
  match play with
  | Done -> 0
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3
;;

let rec run score =
  let opp, me = get_game () in
  let outcome = get_outcome (opp, me) in
  match outcome with
  | End -> score
  | _ ->
    let s = outcome_score outcome in
    let ps = play_score me in
    run score + s + ps
;;

let () =
  let s = 0 in
  let score = run s in
  Printf.printf "%d\n" score
;;

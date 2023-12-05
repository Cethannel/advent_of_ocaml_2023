open Base
open Stdio
module IM = Map.M (Int)

let test_input =
  {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}
;;

let _ = test_input

let get_scores line =
  match String.split_on_chars line ~on:[ ':' ] with
  | [ card; scores ] ->
    (match String.split_on_chars card ~on:[ ' ' ] with
     | v ->
       let num = List.last_exn v in
       let card = Int.of_string num in
       (match String.strip scores |> String.split_on_chars ~on:[ '|' ] with
        | [ w; p ] ->
          let p1 =
            String.split_on_chars w ~on:[ ' ' ]
            |> List.filter ~f:(fun s -> not (String.is_empty s))
            |> List.map ~f:Int.of_string
            |> Set.of_list (module Int)
          in
          let p2 =
            String.split_on_chars p ~on:[ ' ' ]
            |> List.filter ~f:(fun s -> not (String.is_empty s))
            |> List.map ~f:Int.of_string
            |> Set.of_list (module Int)
          in
          card, p1, p2
        | _ -> failwith "bad input"))
  | _ -> failwith "bad input"
;;

let score line ~score_func =
  let _, winner, picked = line in
  let score = Set.inter winner picked |> Set.length in
  score_func score
;;

let get_next line_num score =
  let rec get_next' line_num score acc =
    match score with
    | 0 -> acc
    | _ -> get_next' (line_num + 1) (score - 1) (line_num + 1 :: acc)
  in
  get_next' line_num score []
;;

let rec inc to_change values mult_value =
  match values with
  | [] -> to_change
  | h :: t ->
    inc
      (List.mapi to_change ~f:(fun i v -> if i = h then v + (1 * mult_value) else v))
      t
      mult_value
;;

let play_game cards =
  let scores = List.map cards ~f:(fun card -> score card ~score_func:(fun a -> a)) in
  let empty = List.map scores ~f:(fun _ -> 0) in
  let start = List.map scores ~f:(fun _ -> 1) in
  let rec inner cards i acc score =
    match cards with
    | [] -> acc, score
    | h :: t ->
      let next_things = get_next i @@ List.nth_exn scores i in
      let next_cards = inc acc next_things h in
      inner t (i + 1) next_cards (score + h)
  in
  let rec play to_do score =
    if List.exists to_do ~f:(fun v -> not (v = 0))
    then (
      let to_do, score = inner to_do 0 empty score in
      play to_do score)
    else score
  in
  play start 0
;;

let () =
  let input = In_channel.read_all "input.txt" in
  let cards = String.split_lines input |> List.map ~f:get_scores in
  let scores =
    List.map
      cards
      ~f:
        (score ~score_func:(fun l ->
           match l with
           | 0 -> 0
           | _ -> 1 * Int.pow 2 (l - 1)))
  in
  let total = List.fold scores ~init:1 ~f:( + ) in
  printf "Part 1: %d\n" total;
  let out = play_game cards in
  printf "Part 2: %d\n" out
;;

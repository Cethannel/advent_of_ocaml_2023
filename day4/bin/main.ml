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

let play_game cards =
  let rec get_next index n =
    match n with
    | 0 -> []
    | _ -> index :: get_next (index + 1) (n - 1)
  in
  let round_score =
    List.map cards ~f:(fun line ->
      let i, _, _ = line in
      i, score line ~score_func:(fun l -> l))
    |> Map.of_alist_exn (module Int)
  in
  let rec play_game_hashmap cards acc =
    Map.to_sequence cards
    |> Sequence.to_list
    |> List.map ~f:(fun (k, v) ->
      let score = Map.find_exn round_score k in
      let next = get_next k score in
      let next = List.init v ~f:(fun _ -> next) |> List.concat in
      let rec update_map cards map =
        match cards with
        | [] -> map
        | h :: t ->
          Map.update map h ~f:(fun v ->
            match v with
            | Some v -> v + 1
            | None -> 1)
          |> update_map t
      in
      let new_cards = update_map next cards in
      play_game_hashmap new_cards (acc + 1))
    |> List.fold ~init:acc ~f:( + )
  in
  let cards =
    List.map cards ~f:(fun (i, _, _) -> i, 0) |> Map.of_alist_exn (module Int)
  in
  play_game_hashmap cards 0
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

open Base
open Stdio

let test_input = 
{|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483|}

let _ = test_input

type hand =
  | Five_of_a_kind of char
  | Four_of_a_kind of string
  | Full_house of string
  | Three_of_a_kind of string
  | Two_pair of string
  | One_pair of string
  | High_card of string
;;

let order_of_hand = function
  | Five_of_a_kind _ -> 1
  | Four_of_a_kind _ -> 2
  | Full_house _ -> 3
  | Three_of_a_kind _ -> 4
  | Two_pair _ -> 5
  | One_pair _ -> 6
  | High_card _ -> 7
;;

let to_hand string ~pp =
  let thing = Map.empty (module Char) in
  let rec loop chars map =
    match chars with
    | [] -> map
    | hd :: tl ->
      let count = Map.find map hd |> Option.value ~default:0 in
      loop tl (Map.set map ~key:hd ~data:(count + 1))
  in
  let map = loop (String.to_list string) thing in
  let map = pp map in
  match Map.data map |> List.sort ~compare:Int.compare |> List.rev with
  | [ 5 ] -> Five_of_a_kind (String.to_list string |> List.hd_exn)
  | [ 4; 1 ] -> Four_of_a_kind string
  | [ 3; 2 ] -> Full_house string
  | 3 :: _ -> Three_of_a_kind string
  | 2 :: 2 :: _ -> Two_pair string
  | 2 :: _ -> One_pair string
  | _ -> High_card string
;;

let card_to_score_part1 card =
  match card with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | c -> Char.to_int c - 48
;;

let card_to_score_part2 card =
  match card with
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> -100
  | 'T' -> 10
  | c -> Char.to_int c - 48
;;

let comp_card left right ~card_to_score =
  let left_score = card_to_score left in
  let right_score = card_to_score right in
  if left_score < right_score then
    -1
  else if left_score > right_score then
    1
  else
    0
;;

let split_line line =
  let split = String.split ~on:' ' line in
  let hand = List.hd_exn split in
  let value = List.nth_exn split 1 in
  (hand, value)

let order left right ~card_to_score ~pp =
  let left_hand = to_hand ~pp left in
  let right_hand = to_hand ~pp right in
  let left_order = order_of_hand left_hand in
  let right_order = order_of_hand right_hand in
  if left_order < right_order then
    1
  else if left_order > right_order then
    -1
  else
    let left_list = String.to_list left in
    let right_list = String.to_list right in
    let rec loop left right =
      match left, right with
      | [], [] -> 0
      | lh :: rt, rh :: rr ->
          let comp = comp_card ~card_to_score lh rh in
          if comp = 0 then
            loop rt rr
          else
            comp
      | _ -> failwith "bad input"
    in
    loop left_list right_list

let convert_jokers map =
  let joker_count = Map.find map 'J' |> Option.value ~default:0 in
  let map = Map.remove map 'J' in
  let max = Map.to_sequence map |> Sequence.to_list |> List.max_elt ~compare:(fun (_, l) (_, r) -> Int.compare l r) in
  match max with
  | None -> Map.set map ~key:'2' ~data:joker_count
  | Some (max_idx, max_val) ->
  let map = Map.set map ~key:max_idx ~data:(max_val + joker_count) in
  map
;;

let () = 
  let input = In_channel.read_all "input.txt" in
  let lines = String.split_lines input |> List.map ~f:split_line  in
  let sorted = List.sort lines ~compare:(fun (l, _) (r, _) -> order l r ~card_to_score:card_to_score_part1 ~pp:(fun a -> a)) in
  let result = List.mapi sorted ~f:(fun i (_, v) -> (i+1) * Int.of_string v) in
  let out = List.fold result ~init:0 ~f:(+) in
  printf "%d\n" out;
  let lines = String.split_lines input |> List.map ~f:split_line  in
  let sorted = List.sort lines ~compare:(fun (l, _) (r, _) -> order l r ~card_to_score:card_to_score_part2 ~pp:convert_jokers) in
  List.iter sorted ~f:(fun (l, r) -> printf "%s %s\n" l r);
  let result = List.mapi sorted ~f:(fun i (_, v) -> (i+1) * Int.of_string v) in
  let out = List.fold result ~init:0 ~f:(+) in
  printf "%d\n" out;

open Base
open Stdio

type direction =
  | L
  | R

let test_input =
  {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
|}
;;

let test_input_2 = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
|}

let part_2_test_input =
  {|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
|}
;;

let get_directions input =
  String.split_lines input
  |> List.hd_exn
  |> String.to_list
  |> List.map ~f:(fun c -> if Char.(c = 'L') then L else R)
;;

let get_rules input = String.split_lines input |> List.tl_exn |> List.tl_exn

let convert_rules rules =
  let rec loop rules map =
    match rules with
    | [] -> map
    | rule :: rest ->
      let parts = String.split rule ~on:'=' in
      let key = String.strip (List.hd_exn parts) in
      let values = String.split (List.hd_exn (List.tl_exn parts)) ~on:',' in
      let values =
        List.map
          values
          ~f:(String.strip ~drop:(fun c -> Char.(is_whitespace c || c = '(' || c = ')')))
      in
      (match values with
       | [ left; right ] -> loop rest (Map.set map ~key ~data:(left, right))
       | _ -> failwith "invalid rule")
  in
  loop rules @@ Map.empty (module String)
;;

let walk rules directions =
  let rec loop rules directions location count =
    if String.(location = "ZZZ")
    then count
    else (
      let left, right = Map.find_exn rules location in
      match directions with
      | [] -> failwith "out of directions"
      | direction :: rest ->
        let location =
          match direction with
          | L -> left
          | R -> right
        in
        loop rules (rest @ [ direction ]) location (count + 1))
  in
  loop rules directions "AAA" 0
;;

let gen_rules_and_directions input =
  let directions = get_directions input in
  let rules = get_rules input in
  let rules = convert_rules rules in
  rules, directions
;;

let () =
  let rules, directions = gen_rules_and_directions test_input in
  let count = walk rules directions in
  assert (count = 2)
;;

let () =
  let rules, directions = gen_rules_and_directions test_input_2 in
  let count = walk rules directions in
  assert (count = 6)
;;

let get_last string = String.to_list string |> List.last_exn |> String.of_char
let ends_with ch string = String.(get_last string = Char.to_string ch)

let lcm a b = 
  let rec gcd a b =
    if b = 0 then a else gcd b (a % b)
  in
  (a * b) / (gcd a b)

let walk_part2 rules directions =
  let starts = Map.keys rules |> List.filter ~f:(fun key -> ends_with 'A' key) in
  let rec loop directions location count =
    if ends_with 'Z' location
    then count
    else (
      let left, right = Map.find_exn rules location in
      match directions with
      | [] -> failwith "out of directions"
      | direction :: rest ->
        let location =
          match direction with
          | L -> left
          | R -> right
        in
        loop (rest @ [ direction ]) location (count + 1))
  in
  let cycles = List.map starts ~f:(fun start -> loop directions start 0) in
  List.fold cycles ~init:1 ~f:lcm
;;

let () =
  let rules, directions = gen_rules_and_directions part_2_test_input in
  let count = walk_part2 rules directions in
  assert (count = 6)
;;

let () =
  let rules, directions = gen_rules_and_directions (In_channel.read_all "input.txt") in
  let count = walk rules directions in
  printf "Part1: %d\n" count;
  let count = walk_part2 rules directions in
  printf "Part2: %d\n" count
;;

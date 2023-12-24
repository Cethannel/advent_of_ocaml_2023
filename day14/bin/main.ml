open Base
open Stdio

type rock =
  | Empty
  | Round
  | Square
[@@deriving sexp, compare]

let rock_equal a b =
  match a, b with
  | Empty, Empty -> true
  | Round, Round -> true
  | Square, Square -> true
  | _ -> false
;;

let from_char = function
  | '.' -> Empty
  | 'O' -> Round
  | '#' -> Square
  | _ -> failwith "invalid rock"
;;

let to_char = function
  | Empty -> '.'
  | Round -> 'O'
  | Square -> '#'
;;

let _ = to_char

type direction =
  | Left
  | Right

let _ = Right
let _ = Left

let gravity rocks ~direction =
  let rocks = if Stdlib.(direction = Left) then rocks else List.rev rocks in
  let rec gravity' rocks =
    match rocks with
    | [] -> []
    | Round :: Empty :: rest -> Empty :: gravity' (Round :: rest)
    | h :: t -> h :: gravity' t
  in
  let rec is_stable rocks =
    match rocks with
    | [] -> true
    | Round :: Empty :: _ -> false
    | _ :: t -> is_stable t
  in
  let rec loop rocks =
    let rocks' = gravity' rocks in
    if is_stable rocks' then rocks' else loop rocks'
  in
  let out = loop rocks in
  if Stdlib.(direction = Left) then out else List.rev out
;;

let score rocks =
  List.rev rocks
  |> List.mapi ~f:(fun i row ->
    List.fold row ~init:0 ~f:(fun acc rock ->
      match rock with
      | Empty -> acc
      | Round -> acc + (i + 1)
      | Square -> acc))
  |> List.fold ~init:0 ~f:( + )
;;

let part1 input =
  let lines = String.split_lines input in
  let lines = List.map lines ~f:String.to_list in
  let rocks = List.map lines ~f:(fun line -> List.map line ~f:from_char) in
  let rocks = List.transpose_exn rocks in
  let rocks = List.map rocks ~f:(gravity ~direction:Right) in
  let rocks = List.transpose_exn rocks in
  score rocks
;;

(* Define a comparator module for rock list list *)
module RockListListComparator = struct
  type t = rock list list [@@deriving sexp, compare]
end

(* Create a map module for rock list list *)
module RockListListMap = Stdlib.Map.Make (RockListListComparator)

let cycle rocks cache =
  match RockListListMap.find_opt rocks cache with
  | Some rocks' ->
    (*printf "found in cache\n";*)
    rocks', cache
  | None ->
    let rocks' = List.transpose_exn rocks in
    let rocks' = List.map rocks' ~f:(gravity ~direction:Right) in
    let rocks' = List.transpose_exn rocks' in
    let rocks' = List.map rocks' ~f:(gravity ~direction:Right) in
    let rocks' = List.transpose_exn rocks' in
    let rocks' = List.map rocks' ~f:(gravity ~direction:Left) in
    let rocks' = List.transpose_exn rocks' in
    let rocks' = List.map rocks' ~f:(gravity ~direction:Left) in
    let cache = RockListListMap.add rocks rocks' cache in
    rocks', cache
;;

let part2 input num_loops =
  let lines = String.split_lines input in
  let lines = List.map lines ~f:String.to_list in
  let rocks = List.map lines ~f:(fun line -> List.map line ~f:from_char) in
  let cycle_cache = RockListListMap.empty in
  let loop_cache = [] in
  let rec loop rocks cycle_cache loop_cache n =
    if n = 0
    then rocks
    else (
      printf "n: %d\n" n;
      let rocks', cycle_cache' = cycle rocks cycle_cache in
      match
        List.find loop_cache ~f:(fun (v, _) ->
          if List.for_all2_exn ~f:(List.for_all2_exn ~f:rock_equal) rocks' v
          then true
          else false)
      with
      | Some (_, _) when n < 10 ->
        let loop_cache' = (rocks', n) :: loop_cache in
        loop rocks' cycle_cache' loop_cache' (n - 1)
      | Some (_, n') ->
        let cycle_length = abs (n - n') in
        let remaining_loops = n % cycle_length in
        loop rocks' cycle_cache' loop_cache (remaining_loops - 1)
      | None ->
        let loop_cache' = (rocks', n) :: loop_cache in
        loop rocks' cycle_cache' loop_cache' (n - 1))
  in
  let rocks = loop rocks cycle_cache loop_cache num_loops in
  score rocks
;;

let test_input =
  {|O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....|}
;;

let () =
  let out1 = part1 test_input in
  assert (out1 = 136);
  let out2 = part2 test_input 1000000000 in
  printf "Part 2: %d\n" out2;
  assert (out2 = 64);
;;

let () =
  let input = In_channel.read_all "input.txt" in
  let out1 = part1 input in
  printf "Part 1: %d\n" out1;
  let out2 = part2 input 1000000000 in
  printf "Part 2: %d\n" out2
;;

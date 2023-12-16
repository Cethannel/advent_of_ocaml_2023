open Base
open Stdio

let find_rows map =
  List.filter_mapi
    ~f:(fun i row ->
      if List.for_all ~f:(fun row -> Char.(row = '.')) row then Some i else None)
    map
;;

let find_cols map =
  let map = List.transpose_exn map in
  List.filter_mapi
    ~f:(fun i row ->
      if List.for_all ~f:(fun row -> Char.(row = '.')) row then Some i else None)
    map
;;

let find_galaxies map =
  let index = ref 0 in
  List.mapi map ~f:(fun i row ->
    List.filter_mapi row ~f:(fun j col ->
      if Char.(col = '#')
      then (
        index := !index + 1;
        Some (!index, (j, i)))
      else None))
  |> List.concat
;;

let calc_distance ?(expantion_factor = 2) rows cols (x1, y1) (x2, y2) =
  let expantion_factor = expantion_factor - 1 in
  let row_add =
    List.filter rows ~f:(fun row -> Int.min x1 x2 < row && row < Int.max x1 x2)
    |> List.fold ~init:0 ~f:(fun acc _ -> acc + expantion_factor)
  in
  let col_add =
    List.filter cols ~f:(fun col -> Int.min y1 y2 < col && col < Int.max y1 y2)
    |> List.fold ~init:0 ~f:(fun acc _ -> acc + expantion_factor)
  in
  Int.abs (x1 - x2) + Int.abs (y1 - y2) + row_add + col_add
;;

let get_pairs galaxies =
  List.cartesian_product galaxies galaxies
  |> List.filter ~f:(fun ((idx1, _), (idx2, _)) -> Int.(idx1 < idx2))
;;

let part1 input =
  let map = String.split_lines input |> List.map ~f:(fun row -> String.to_list row) in
  let rows = find_rows map in
  let cols = find_cols map in
  let galaxies = find_galaxies map in
  let pairs = get_pairs galaxies in
  let distances =
    List.map pairs ~f:(fun ((idx1, (x1, y1)), (idx2, (x2, y2))) ->
      (idx1, idx2), calc_distance cols rows (x1, y1) (x2, y2))
  in
  List.fold distances ~init:0 ~f:(fun acc (_, dist) -> acc + dist)
;;

let part2 input expantion_factor =
  let map = String.split_lines input |> List.map ~f:(fun row -> String.to_list row) in
  let rows = find_rows map in
  let cols = find_cols map in
  let galaxies = find_galaxies map in
  let pairs = get_pairs galaxies in
  let distances =
    List.map pairs ~f:(fun ((idx1, (x1, y1)), (idx2, (x2, y2))) ->
      (idx1, idx2), calc_distance ~expantion_factor cols rows (x1, y1) (x2, y2))
  in
  List.fold distances ~init:0 ~f:(fun acc (_, dist) -> acc + dist)
;;

let test_input =
  {|...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....|}
;;

let () =
  let part1 = part1 test_input in
  assert (part1 = 374);
  let part2 = part2 test_input 100 in
  assert (part2 = 8410);
  print_endline "Tests passed"
;;

let () =
  let input = In_channel.read_all "input.txt" in
  printf "Part 1: %d\n" (part1 input);
  print_endline "Part 2 is too high";
  printf "Part 2: %d\n" (part2 input 1000000)
;;

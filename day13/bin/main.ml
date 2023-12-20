open Base
open Stdio

let test_input =
  {|#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#|}
;;

let check lines =
  List.mapi lines ~f:(fun i _ ->
    let top, bottom = List.split_n lines i in
    let min = Int.min (List.length top) (List.length bottom) in
    let top = List.rev top in
    let top = List.take top min in
    let bottom = List.take bottom min in
    if List.for_all2_exn top bottom ~f:(fun a b -> String.(a = b)) then i else 0)
;;

let diffs str1 str2 =
  let str1 = String.to_list str1 in
  let str2 = String.to_list str2 in
  List.map2_exn str1 str2 ~f:(fun a b -> Char.(a <> b))
  |> List.filter ~f:Fn.id
  |> List.length
;;

let () =
  let diffs = diffs "#.##..##." "..##..##." in
  assert (diffs = 1);
;;

let check_part2 lines =
  List.mapi lines ~f:(fun i _ ->
    let top, bottom = List.split_n lines i in
    let min = Int.min (List.length top) (List.length bottom) in
    let top = List.rev top in
    let top = List.take top min in
    let bottom = List.take bottom min in
    let diffs = List.map2_exn top bottom ~f:(diffs) |> List.sum (module Int) ~f:Fn.id in
    if diffs = 1 then i else 0)
;;

let process_area lines ~check =
  let lines = List.filter lines ~f:(fun line -> String.(line <> "")) in
  let cols = List.map lines ~f:String.to_list in
  let cols = List.transpose_exn cols in
  let cols = List.map cols ~f:(fun col -> String.of_char_list col) in
  let cols = check cols in
  let lines = check lines in
  let cols = List.find cols ~f:(fun v -> Int.(v <> 0)) in
  let lines = List.find lines ~f:(fun v -> Int.(v <> 0)) in
  match cols with
  | None -> Option.value_exn lines * 100
  | Some cols -> cols
;;

let part_1 input =
  let lines = String.split_lines input in
  let areas = List.group lines ~break:(fun _ b -> String.(b = "")) in
  List.map areas ~f:(process_area ~check) |> List.fold ~init:0 ~f:( + )
;;

let part_2 input =
  let lines = String.split_lines input in
  let areas = List.group lines ~break:(fun _ b -> String.(b = "")) in
  List.map areas ~f:(process_area ~check:check_part2) |> List.fold ~init:0 ~f:( + )
;;

let () =
  let out = part_1 test_input in
  assert (out = 405);
  let out = part_2 test_input in
  assert (out = 400);
;;

let () =
  let input = In_channel.read_all "input.txt" in
  let out = part_1 input in
  printf "%d\n" out;
  assert (out = 30487);
  let out = part_2 input in
  printf "%d\n" out;
  assert (out = 31954);
;;

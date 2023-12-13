open Base
open Stdio

let test_input = {|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45|}

let process_line line = String.split line ~on:' ' |> List.map ~f:Int.of_string

let get_next_row line =
  let rec get_next_row' line acc =
    match line with
    | [] -> List.rev acc
    | [ _ ] -> List.rev acc
    | f :: (s :: _ as rest) ->
      let diff = s - f in
      get_next_row' rest (diff :: acc)
  in
  let out = get_next_row' line [] in
  out
;;

let gen_next row =
  let rec gen_next' row =
    match row with
    | _ when List.for_all row ~f:(( = ) 0) -> 0
    | _ -> List.last_exn row + (gen_next' @@ get_next_row row)
  in
  gen_next' row
;;

let part_1 input =
  let lines = String.split_lines input in
  let lines = List.map lines ~f:process_line in
  let lines = List.map lines ~f:gen_next in
  List.fold lines ~init:0 ~f:( + )
;;

let part_2 input =
  let lines = String.split_lines input in
  let lines = List.map lines ~f:process_line in
  let lines = List.map lines ~f:List.rev in
  let lines = List.map lines ~f:gen_next in
  List.fold lines ~init:0 ~f:( + )
;;

let () =
  let input = test_input in
  let output = part_1 input in
  assert (output = 114)
;;

let () =
  let input = test_input in
  let output = part_2 input in
  assert (output = 2)
;;

let () =
  let input = In_channel.read_all "input.txt" in
  let output = part_1 input in
  printf "Part 1: %d\n" output;
  let output = part_2 input in
  printf "Part 2: %d\n" output
;;

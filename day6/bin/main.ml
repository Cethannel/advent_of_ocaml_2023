open Base
open Stdio

let test_input =
  {|Time:      7  15   30
Distance:  9  40  200|}

let _ = test_input

let get_nums line =
  let times = String.split line ~on:':' in
  let times = String.split (List.nth_exn times 1) ~on:' ' |> List.filter ~f:(fun x -> not (String.is_empty x)) in
  times

let get_times input =
  let first_line = String.split_lines input |> List.hd_exn in
  get_nums first_line

let get_distances input =
  let second_line = String.split_lines input in
  get_nums @@ List.nth_exn second_line 1

let time_to_distance time hold_time =
  (time - hold_time) * hold_time

let () =
  let input = In_channel.read_all "input.txt" in
  let times = get_times input |> List.map ~f:(Int.of_string) in
  let distances = get_distances input |> List.map ~f:(Int.of_string) in
  let zipped = List.zip_exn times distances in
  let out = List.map zipped ~f:(fun (time, distance) ->
    List.init time ~f:(fun x -> time_to_distance time x)
    |> List.count ~f:(fun x -> x > distance)
  ) in
  let mult = List.fold out ~init:1 ~f:( * ) in
  printf "%d\n" mult;
  let time = get_times input |> String.concat ~sep:"" |> Int.of_string in
  let distances = get_distances input |> String.concat ~sep:"" |> Int.of_string in
  let out = List.init time ~f:(fun x -> time_to_distance time x) in
  let out = List.count out ~f:(fun x -> x > distances) in
  printf "%d\n" out


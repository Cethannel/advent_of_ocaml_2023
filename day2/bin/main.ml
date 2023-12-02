open Base
open Stdio

type cubes =
  { red : int
  ; green : int
  ; blue : int
  }

let default_cubes = { red = 0; green = 0; blue = 0 }
let maxes = { red = 12; green = 13; blue = 14 }

let unwrap = function
  | Some x -> x
  | None -> failwith "unwrap failed"
;;

let update_cubes cubes color num =
  match color with
  | "red" -> { cubes with red = cubes.red + num }
  | "green" -> { cubes with green = cubes.green + num }
  | "blue" -> { cubes with blue = cubes.blue + num }
  | _ -> cubes
;;

let max_cube cube1 cube2 =
  { red = Int.max cube1.red cube2.red
  ; green = Int.max cube1.green cube2.green
  ; blue = Int.max cube1.blue cube2.blue
  }
;;

let filter_cube cube =
  cube.red <= maxes.red && cube.green <= maxes.green && cube.blue <= maxes.blue
;;

let process_cube str =
  let split = String.split_on_chars ~on:[ ' ' ] str in
  match split with
  | [] -> None
  | [ _; num; color ] ->
    let num = Int.of_string num in
    Some (color, num)
  | _ -> None
;;

let split_line line =
  (* split line at on : *)
  let split = String.split_on_chars ~on:[ ':' ] line in
  match split with
  | [] -> failwith "split_line failed"
  | game :: cubes ->
    let game_num =
      String.split_on_chars ~on:[ ' ' ] game |> List.last |> unwrap |> Int.of_string
    in
    let cubes = List.hd cubes |> unwrap in
    let turns = String.split_on_chars ~on:[ ';' ] cubes in
    let process_turn turn =
      let split = String.split_on_chars ~on:[ ',' ] turn in
      let cubes = List.map ~f:process_cube split |> List.filter_opt in
      List.fold cubes ~init:default_cubes ~f:(fun acc (color, num) ->
        update_cubes acc color num)
    in
    let turns = List.map ~f:process_turn turns in
    game_num, List.fold turns ~init:default_cubes ~f:max_cube
;;

let get_power cube = cube.red * cube.green * cube.blue

let () =
  let lines = In_channel.read_lines "input.txt" in
  let lines =
    List.filter ~f:(fun a -> String.length a > 0) lines |> List.map ~f:split_line
  in
  List.filter lines ~f:(fun (_, cube) -> filter_cube cube)
  |> List.fold ~init:0 ~f:(fun acc (game_num, _) -> acc + game_num)
  |> printf "Part 1: %d\n";
  List.map lines ~f:(fun (_, cube) -> get_power cube)
  |> List.fold ~init:0 ~f:( + )
  |> printf "Part 2: %d\n"
;;

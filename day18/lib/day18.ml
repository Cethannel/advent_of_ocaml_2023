open Base
open Stdio

type direction =
  | Up
  | Down
  | Left
  | Right

let parse_direction = function
  | 'U' -> Up
  | 'D' -> Down
  | 'L' -> Left
  | 'R' -> Right
  | _ -> failwith "Invalid direction"
;;

type pos =
  { x : int
  ; y : int
  }
[@@deriving sexp, compare]

module Pos = struct
  module T = struct
    type t = pos [@@deriving sexp, compare]
  end

  include T
  include Comparable.Make (T)
end

type tile =
  | Inside
  | Outside
  | Wall
[@@deriving sexp]

type move =
  { offset : int * int
  ; distance : int
  }

let num_points outeline border_length =
  (* Shoe lace formula *)
  let rec loop acc i =
    if i = List.length outeline - 1
    then acc
    else (
      let x1, y1 = List.nth_exn outeline i in
      let x2, y2 = List.nth_exn outeline (i + 1) in
      loop (acc + (x1 * y2) - (x2 * y1)) (i + 1))
  in
  let area = loop 0 0 / 2 |> Float.of_int in
  let f = Float.(Float.abs area - (0.5 * Float.of_int border_length) + 1.0) in
  border_length + Float.to_int f
;;

let add_pos (x1, y1) (x2, y2) = x1 + x2, y1 + y2

let solve moves =
  let rec loop moves outline border_length =
    match moves with
    | [] -> outline, border_length
    | { offset; distance } :: t ->
      let scaled_offset = fst offset * distance, snd offset * distance in
      let prev = List.hd_exn outline in
      let added = add_pos prev scaled_offset in
      let new_outline = added :: outline in
      let new_border_length = border_length + distance in
      loop t new_outline new_border_length
  in
  let outline, border_length = loop moves [ 0, 0 ] 0 in
  let outline = List.rev outline in
  let num_points = num_points outline border_length in
  num_points
;;

let parse_line line =
  let direction = parse_direction @@ String.get line 0 in
  let distance = String.split line ~on:' ' in
  let distance = List.nth_exn distance 1 |> Int.of_string in
  let offset =
    match direction with
    | Up -> 0, 1
    | Down -> 0, -1
    | Left -> -1, 0
    | Right -> 1, 0
  in
  { offset; distance }
;;

let part1 input =
  let lines = String.split_lines input in
  let moves = List.map lines ~f:parse_line in
  solve moves
;;

let test_input =
  {|R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)|}
;;

let%test "part1" =
  let result = part1 test_input in
  printf "%d\n" result;
  [%compare.equal: int] result 62
;;

let direction_from_char = function
  | '0' -> Right
  | '1' -> Down
  | '2' -> Left
  | '3' -> Up
  | _ -> failwith "Invalid direction"
;;

let parse_line line =
  let parts = String.split line ~on:' ' in
  let data = List.last_exn parts in
  let hex = String.sub data ~pos:1 ~len:(String.length data - 2) in
  let direction = direction_from_char @@ String.get data @@ String.length hex in
  let distance = String.sub data ~pos:2 ~len:5 in
  let distance = Int.of_string @@ "0x" ^ distance in
  let offset =
    match direction with
    | Up -> 0, 1
    | Down -> 0, -1
    | Left -> -1, 0
    | Right -> 1, 0
  in
  { offset; distance }
;;

let part2 input =
  let lines = String.split_lines input in
  let moves = List.map lines ~f:parse_line in
  solve moves
;;

let%test "part2" =
  let result = part2 test_input in
  printf "%d\n" result;
  [%compare.equal: int] result 952408144115
;;

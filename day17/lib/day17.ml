open Base
open Stdio

type direction =
  | Up
  | Down
  | Left
  | Right

let direction_to_string = function
  | Up -> "^"
  | Down -> "v"
  | Left -> "<"
  | Right -> ">"
;;

let _ = direction_to_string

let is_oposite d1 d2 =
  match d1, d2 with
  | Up, Down -> true
  | Down, Up -> true
  | Left, Right -> true
  | Right, Left -> true
  | _ -> false
;;

let _ = is_oposite

let same_direction d1 d2 =
  match d1, d2 with
  | Up, Up -> true
  | Down, Down -> true
  | Left, Left -> true
  | Right, Right -> true
  | _ -> false
;;

type turn_direction =
  | Left
  | Right
  | Straight

type state =
  { x : int
  ; y : int
  ; direction : direction
  ; num_steps : int
  ; heat_loss : int
  }

let gen_next_direction state next_direction =
  let { x; y; direction; num_steps; _ } = state in
  match next_direction with
  | Straight ->
    (match direction with
     | Up -> { state with y = y - 1; num_steps = num_steps + 1 }
     | Down -> { state with y = y + 1; num_steps = num_steps + 1 }
     | Left -> { state with x = x - 1; num_steps = num_steps + 1 }
     | Right -> { state with x = x + 1; num_steps = num_steps + 1 })
  | Left ->
    (match direction with
     | Up -> { state with x = x - 1; direction = Left; num_steps = 1 }
     | Down -> { state with x = x + 1; direction = Right; num_steps = 1 }
     | Left -> { state with y = y + 1; direction = Down; num_steps = 1 }
     | Right -> { state with y = y - 1; direction = Up; num_steps = 1 })
  | Right ->
    (match direction with
     | Up -> { state with x = x + 1; direction = Right; num_steps = 1 }
     | Down -> { state with x = x - 1; direction = Left; num_steps = 1 }
     | Left -> { state with y = y - 1; direction = Up; num_steps = 1 }
     | Right -> { state with y = y + 1; direction = Down; num_steps = 1 })
;;

module StateCompare = struct
  type t = state

  let compare { heat_loss = hl1; _ } { heat_loss = hl2; _ } = hl1 - hl2
end

module Heap = Containers.Heap.Make_from_compare (StateCompare)

let solve input min max =
  let lines = String.split_lines input in
  let input =
    List.map lines ~f:(fun line ->
      String.to_list line |> List.map ~f:(fun c -> String.of_char c |> Int.of_string))
  in
  let map = Array.of_list_map input ~f:Array.of_list in
  let max_x = Array.length map.(0) - 1 in
  let max_y = Array.length map - 1 in
  let rec loop to_visit visited =
    match Heap.take to_visit with
    | None -> failwith "no solution"
    | Some (to_visit, state) ->
      let { x; y; num_steps; heat_loss; _ } = state in
      if x < 0 || y < 0 || x > max_x || y > max_y
      then loop to_visit visited
      else (
        if heat_loss > 200
        then (
          printf "%d %d %d %d\n" x y num_steps heat_loss;
          printf "%d" List.(length visited);
          print_endline "");
        let new_heat_loss = map.(y).(x) in
        if x = max_x && y = max_y && num_steps >= min
        then heat_loss + new_heat_loss
        else if List.mem
                  visited
                  state
                  ~equal:
                    (fun
                      { x; y; num_steps; direction; _ }
                      { x = x'
                      ; y = y'
                      ; num_steps = num_steps'
                      ; direction = direction'
                      ; _
                      }
                    ->
                    x = x'
                    && y = y'
                    && num_steps = num_steps'
                    && same_direction direction direction')
        then loop to_visit visited
        else (
          let state = { state with heat_loss = heat_loss + new_heat_loss } in
          let visited = state :: visited in
          let to_visit =
            if num_steps >= min
            then Heap.add to_visit (gen_next_direction state Left)
            else to_visit
          in
          let to_visit =
            if num_steps >= min
            then Heap.add to_visit (gen_next_direction state Right)
            else to_visit
          in
          let to_visit =
            if num_steps < max
            then Heap.add to_visit (gen_next_direction state Straight)
            else to_visit
          in
          loop to_visit visited))
  in
  let start_state =
    [ { x = 0; y = 0; direction = Right; num_steps = 1; heat_loss = 0 }
    ; { x = 0; y = 0; direction = Down; num_steps = 1; heat_loss = 0 }
    ]
  in
  let heap = Heap.of_list start_state in
  let heat_loss = loop heap [] in
  heat_loss - map.(0).(0)
;;

let part1 input = solve input 0 3
let part2 input = solve input 4 10

let example_input =
  {|2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533|}
;;

let test_part1 () =
  let out = part1 example_input in
  printf "%d\n" out;
  assert (out = 102)
;;

let%test "example 1" =
  let out = part1 example_input in
  printf "%d\n" out;
  out = 102
;;

let%test "example 2" = part2 example_input = 94

let example_input2 = {|111111111111
999999999991
999999999991
999999999991
999999999991|}

let _ = example_input2
let%test "example 3" = part2 example_input2 = 71

open Base
open Stdio

let _ = printf

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

let is_oposite d1 d2 =
  match d1, d2 with
  | Up, Down -> true
  | Down, Up -> true
  | Left, Right -> true
  | Right, Left -> true
  | _ -> false
;;

let same_direction d1 d2 =
  match d1, d2 with
  | Up, Up -> true
  | Down, Down -> true
  | Left, Left -> true
  | Right, Right -> true
  | _ -> false
;;

type state =
  { x : int
  ; y : int
  ; direction : direction
  ; num_steps : int
  }

let part1 input =
  let lines = String.split_lines input in
  let input =
    List.map lines ~f:(fun line ->
      String.to_list line |> List.map ~f:(fun c -> String.of_char c |> Int.of_string))
  in
  let map = Array.of_list_map input ~f:Array.of_list in
  let max_x = Array.length map.(0) - 1 in
  let max_y = Array.length map - 1 in
  let last_direction = ref Right in
  let rec loop state searched poses =
    let { x; y; num_steps; direction } = state in
    if same_direction direction !last_direction
    then print_endline @@ direction_to_string direction;
    last_direction := direction;
    let heat_loss = map.(y).(x) in
    let searched = (x, y) :: searched in
    if x = max_x && y = max_y
    then (x, y) :: poses, searched, heat_loss
    else (
      let to_search =
        [ { x = x + 1; y; direction = Right; num_steps = num_steps + 1 }
        ; { x = x - 1; y; direction = Left; num_steps = num_steps + 1 }
        ; { x; y = y + 1; direction = Down; num_steps = num_steps + 1 }
        ; { x; y = y - 1; direction = Up; num_steps = num_steps + 1 }
        ]
      in
      let to_search =
        List.map to_search ~f:(fun new_state ->
          let { direction; num_steps; _ } = new_state in
          if same_direction direction state.direction
          then { new_state with num_steps = num_steps + 1 }
          else { new_state with num_steps = 1 })
      in
      let to_search =
        List.filter to_search ~f:(fun { direction; _ } ->
          not (is_oposite direction state.direction))
      in
      let to_search =
        List.filter to_search ~f:(fun { x; y; _ } ->
          x >= 0 && x <= max_x && y >= 0 && y <= max_y)
      in
      let to_search =
        List.filter to_search ~f:(fun { x; y; _ } ->
          not
            (List.mem searched (x, y) ~equal:(fun (x1, y1) (x2, y2) -> x1 = x2 && y1 = y2)))
      in
      let to_search =
        List.filter to_search ~f:(fun { direction; num_steps; _ } ->
          if same_direction direction state.direction then num_steps <= 3 else true)
      in
      if List.is_empty to_search
      then (x, y) :: poses, searched, Int.max_value
      else (
        let poses, searched', new_heat_loss =
          List.fold
            to_search
            ~init:(poses, searched, Int.max_value)
            ~f:(fun (poses, searched, heat_loss) state ->
              let poses', searched', heat_loss' = loop state searched ((x, y) :: poses) in
              if heat_loss' < heat_loss
              then poses', searched', heat_loss'
              else poses, searched, heat_loss)
        in
        if new_heat_loss = Int.max_value
        then poses, searched, heat_loss
        else poses, searched', heat_loss + new_heat_loss))
  in
  let poses, _, heat_loss =
    loop { x = 0; y = 0; direction = Right; num_steps = 1 } [] [ 0, 0 ]
  in
  Array.iteri map ~f:(fun y row ->
    Array.iteri row ~f:(fun x heat ->
      if List.mem poses (x, y) ~equal:(fun (x1, y1) (x2, y2) -> x1 = x2 && y1 = y2)
      then printf "O"
      else printf "%d" heat);
    print_endline "");
  heat_loss
;;

let part2 _input = 0

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

let%test "example 1" =
  let out = part1 example_input in
  printf "%d\n" out;
  out = 102
;;

let%test "example 2" = part2 example_input = 202

open Base
open Stdio
open Parmap

let _ = printf

type direction =
  | Left
  | Right
  | Up
  | Down
[@@deriving sexp, compare]

type lazer =
  { x : int
  ; y : int
  ; direction : direction
  }
[@@deriving sexp, compare]

let process_lazers ~grid ~start_lazer =
  let lazers = [ start_lazer ] in
  let energized =
    Array.make_matrix ~dimx:(Array.length grid.(0)) ~dimy:(Array.length grid) false
  in
  let lazer_prev = ref [] in
  let rec loop lazers =
    match lazers with
    | [] -> ()
    | lazer :: t ->
      if List.mem !lazer_prev lazer ~equal:(fun a b -> compare_lazer a b = 0)
      then loop t
      else (
        lazer_prev := lazer :: !lazer_prev;
        if lazer.x >= 0
           && lazer.y >= 0
           && lazer.x < Array.length grid.(0)
           && lazer.y < Array.length grid
        then energized.(lazer.y).(lazer.x) <- true;
        match lazer.direction with
        | Left ->
          if lazer.x = 0
          then loop t
          else (
            let new_lazer = { lazer with x = lazer.x - 1 } in
            match grid.(new_lazer.y).(new_lazer.x) with
            | '.' | '-' -> loop (t @ [ new_lazer ])
            | '\\' -> loop (t @ [ { new_lazer with direction = Up } ])
            | '/' -> loop (t @ [ { new_lazer with direction = Down } ])
            | '|' ->
              loop
                (t
                 @ [ { new_lazer with direction = Up }
                   ; { new_lazer with direction = Down }
                   ])
            | _ -> failwith "invalid input")
        | Right ->
          if lazer.x = Array.length grid - 1
          then loop t
          else (
            let new_lazer = { lazer with x = lazer.x + 1 } in
            match grid.(new_lazer.y).(new_lazer.x) with
            | '.' | '-' -> loop (t @ [ new_lazer ])
            | '\\' -> loop (t @ [ { new_lazer with direction = Down } ])
            | '/' -> loop (t @ [ { new_lazer with direction = Up } ])
            | '|' ->
              loop
                (t
                 @ [ { new_lazer with direction = Up }
                   ; { new_lazer with direction = Down }
                   ])
            | _ -> failwith "invalid input")
        | Up ->
          if lazer.y = 0
          then loop t
          else (
            let new_lazer = { lazer with y = lazer.y - 1 } in
            match grid.(new_lazer.y).(new_lazer.x) with
            | '.' | '|' -> loop (t @ [ new_lazer ])
            | '\\' -> loop (t @ [ { new_lazer with direction = Left } ])
            | '/' -> loop (t @ [ { new_lazer with direction = Right } ])
            | '-' ->
              loop
                (t
                 @ [ { new_lazer with direction = Left }
                   ; { new_lazer with direction = Right }
                   ])
            | _ -> failwith "invalid input")
        | Down ->
          if lazer.y = Array.length grid.(0) - 1
          then loop t
          else (
            let new_lazer = { lazer with y = lazer.y + 1 } in
            match grid.(new_lazer.y).(new_lazer.x) with
            | '.' | '|' -> loop (t @ [ new_lazer ])
            | '\\' -> loop (t @ [ { new_lazer with direction = Right } ])
            | '/' -> loop (t @ [ { new_lazer with direction = Left } ])
            | '-' ->
              loop
                (t
                 @ [ { new_lazer with direction = Left }
                   ; { new_lazer with direction = Right }
                   ])
            | _ -> failwith "invalid input"))
  in
  loop lazers;
  let count = ref 0 in
  for i = 0 to Array.length energized - 1 do
    for j = 0 to Array.length energized.(0) - 1 do
      if energized.(i).(j) then count := !count + 1 else ()
    done
  done;
  !count
;;

let part1 input =
  let grid = String.split_lines input in
  let grid = List.map grid ~f:String.to_list in
  let grid = Array.of_list_map grid ~f:Array.of_list in
  process_lazers ~grid ~start_lazer:{ x = -1; y = 0; direction = Right }
;;

let part2 input =
  let grid = String.split_lines input in
  let grid = List.map grid ~f:String.to_list in
  let lazers =
    List.mapi grid ~f:(fun y row ->
      let out =
        List.filter_mapi row ~f:(fun x _ ->
          let max_x = List.hd_exn grid |> List.length in
          let max_y = List.length grid in
          match x, y with
          | 0, 0 ->
            Some
              [ { x = -1; y = 0; direction = Right }
              ; { x = 0; y = -1; direction = Down }
              ]
          | 0, _ -> Some [ { x = -1; y; direction = Right } ]
          | _, 0 -> Some [ { x; y = -1; direction = Down } ]
          | x, y when x = max_x - 1 && y = max_y - 1 ->
            Some [ { x = max_x; y; direction = Left }; { x; y = max_y; direction = Up } ]
          | x, y when x = max_x - 1 -> Some [ { x = max_x; y; direction = Left } ]
          | x, y when y = max_y - 1 -> Some [ { x; y = max_y; direction = Up } ]
          | _ -> None)
        |> List.concat
      in
      out)
    |> List.concat
  in
  let grid = Array.of_list_map grid ~f:Array.of_list in
  let outs =
    parmap ~ncores:16 (fun l -> process_lazers ~grid ~start_lazer:l) @@ Parmap.L lazers
  in
  List.max_elt outs ~compare:Int.compare |> Option.value_exn
;;

let test_input =
  {|.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....|}
;;

let%test "test_input_part1" = part1 test_input = 46
let%test "test_input_part2" = part2 test_input = 51

open Base
open Stdio

type pipe =
  { x : int
  ; y : int
  ; next : pipe option
  ; distance : int
  ; ch : char
  }
[@@deriving show]

type direction =
  | Horizontal
  | Vertical
[@@deriving show]

let _ = Vertical
let _ = pp_direction

type cell =
  | Pipe
  | Gap
  | Empty of bool

let show_cell cell =
  match cell with
  | Pipe -> "*"
  | Gap -> "."
  | Empty true -> "█"
  | Empty false -> " "
;;

let _ = show_cell

let to_cell v =
  match v with
  | Some _ -> Pipe
  | None -> Empty true
;;

let _ = show_pipe
let get_pos pipes (x, y) = List.nth_exn (List.nth_exn pipes y) x

let set_pos pipes (x, y) value =
  List.mapi pipes ~f:(fun i row ->
    if i = y then List.mapi row ~f:(fun j pipe -> if j = x then value else pipe) else row)
;;

let () =
  let pipes = [ [ 'a'; 'b'; 'c' ]; [ 'd'; 'e'; 'f' ]; [ 'g'; 'h'; 'i' ] ] in
  assert (Char.(get_pos pipes (0, 0) = 'a'));
  let pipes = set_pos pipes (0, 0) 'z' in
  assert (Char.(get_pos pipes (0, 0) = 'z'))
;;

let get_oposite p v = p + (p - v)

let () =
  let x = 1 in
  let prev_x = 2 in
  assert (get_oposite x prev_x = 0)
;;

let rec gen_next_pipe x y pipes distance (prev_x, prev_y) =
  let next_pos =
    match get_pos pipes (x, y) with
    | '|' ->
      let next_y = get_oposite y prev_y in
      Some (x, next_y)
    | '-' ->
      let next_x = get_oposite x prev_x in
      Some (next_x, y)
    | 'F' ->
      let next_x = if x <> prev_x then x else x + 1 in
      let next_y = if y <> prev_y then y else y + 1 in
      Some (next_x, next_y)
    | 'J' ->
      let next_x = if x <> prev_x then x else x - 1 in
      let next_y = if y <> prev_y then y else y - 1 in
      Some (next_x, next_y)
    | '7' ->
      let next_x = if x <> prev_x then x else x - 1 in
      let next_y = if y <> prev_y then y else y + 1 in
      Some (next_x, next_y)
    | 'L' ->
      let next_x = if x <> prev_x then x else x + 1 in
      let next_y = if y <> prev_y then y else y - 1 in
      Some (next_x, next_y)
    | _ -> None
  in
  match next_pos with
  | Some (next_x, next_y) ->
    let next_pipe = gen_next_pipe next_x next_y pipes (distance + 1) (x, y) in
    Some { x; y; next = next_pipe; distance; ch = get_pos pipes (x, y) }
  | None -> None
;;

let find_s pipes =
  List.find_mapi pipes ~f:(fun y row ->
    List.find_mapi row ~f:(fun x pipe ->
      match pipe with
      | 'S' -> Some (x, y)
      | _ -> None))
;;

let gent_s_pipe x y pipes =
  let top = get_pos pipes (x, y - 1) in
  let bottom = get_pos pipes (x, y + 1) in
  let left = get_pos pipes (x - 1, y) in
  let right = get_pos pipes (x + 1, y) in
  let next1, next2, ch =
    match top, bottom, left, right with
    | ('|' | 'F' | '7'), ('|' | 'L' | 'J'), _, _ ->
      let next_1 = gen_next_pipe x (y + 1) pipes 1 (x, y) in
      let next_2 = gen_next_pipe x (y - 1) pipes 1 (x, y) in
      next_1, next_2, '|'
    | _, _, ('-' | 'F' | 'L'), ('-' | '7' | 'J') ->
      let next_1 = gen_next_pipe (x + 1) y pipes 1 (x, y) in
      let next_2 = gen_next_pipe (x - 1) y pipes 1 (x, y) in
      next_1, next_2, '-'
    | ('|' | 'F' | '7'), _, ('-' | 'L' | 'F'), _ ->
      let next_1 = gen_next_pipe (x - 1) y pipes 1 (x, y) in
      let next_2 = gen_next_pipe x (y - 1) pipes 1 (x, y) in
      next_1, next_2, 'J'
    | _, ('|' | 'L' | 'J'), _, ('-' | '7' | 'J') ->
      let next_1 = gen_next_pipe (x + 1) y pipes 1 (x, y) in
      let next_2 = gen_next_pipe x (y + 1) pipes 1 (x, y) in
      next_1, next_2, 'F'
    | ('|' | 'F' | '7'), _, _, ('-' | '7' | 'J') ->
      let next_1 = gen_next_pipe (x + 1) y pipes 1 (x, y) in
      let next_2 = gen_next_pipe x (y - 1) pipes 1 (x, y) in
      next_1, next_2, 'L'
    | _, ('|' | 'L' | 'J'), ('-' | 'L' | 'F'), _ ->
      let next_1 = gen_next_pipe (x - 1) y pipes 1 (x, y) in
      let next_2 = gen_next_pipe x (y + 1) pipes 1 (x, y) in
      next_1, next_2, '7'
    | _ -> failwith "Invalid S pipe"
  in
  Option.value_exn next1, Option.value_exn next2, ch
;;

let process_input input =
  let input = String.split_lines input in
  let input = List.map input ~f:String.to_list in
  let input = List.init (List.hd_exn input |> List.length) ~f:(fun _ -> '.') :: input in
  let input =
    input @ [ List.init (List.hd_exn input |> List.length) ~f:(fun _ -> '.') ]
  in
  let input = List.map input ~f:(fun row -> [ '.' ] @ row @ [ '.' ]) in
  input
;;

let test_input = {|..F7.
.FJ|.
SJ.L7
|F--J
LJ...|}

let gen_map (s_pipe_1, s_pipe_2) (size_x, size_y) =
  let rec gen_map' pipe map =
    let current = get_pos map (pipe.x, pipe.y) in
    let map =
      match current with
      | None -> set_pos map (pipe.x, pipe.y) (Some pipe.distance)
      | Some distance ->
        if pipe.distance < distance
        then set_pos map (pipe.x, pipe.y) (Some pipe.distance)
        else map
    in
    match pipe.next with
    | None -> map
    | Some next -> gen_map' next map
  in
  let map = List.init size_y ~f:(fun _ -> List.init size_x ~f:(fun _ -> None)) in
  let map = gen_map' s_pipe_1 map in
  let map = gen_map' s_pipe_2 map in
  map
;;

let part1 input =
  let pipes = process_input input in
  List.iter pipes ~f:(fun row ->
    List.iter row ~f:(fun pipe -> printf "%c" pipe);
    print_endline "");
  let s = find_s pipes |> Option.value_exn in
  let s_1, s_2, _ = gent_s_pipe (fst s) (snd s) pipes in
  let map = gen_map (s_1, s_2) (List.hd_exn pipes |> List.length, List.length pipes) in
  List.fold map ~init:0 ~f:(fun acc row ->
    List.fold row ~init:acc ~f:(fun acc pipe ->
      match pipe with
      | None -> acc
      | Some distance -> Int.max acc distance))
;;

let () =
  let test_out = part1 test_input in
  assert (test_out = 8)
;;

let gen_map_2 (s_pipe_1, s_pipe_2) (size_x, size_y) (s_x, s_y) s_val =
  let rec gen_map' pipe map =
    match pipe with
    | None -> map
    | Some pipe -> gen_map' pipe.next (set_pos map (pipe.x, pipe.y) (Some pipe.ch))
  in
  let map = List.init size_y ~f:(fun _ -> List.init size_x ~f:(fun _ -> None)) in
  let map = gen_map' (Some s_pipe_1) map in
  let map = gen_map' (Some s_pipe_2) map in
  let map = set_pos map (s_x, s_y) (Some s_val) in
  map
;;

let rec window list ~f =
  match list with
  | [] -> []
  | _ :: [] -> []
  | x :: y :: xs -> f x y :: window (y :: xs) ~f
;;

let get_center ~dir first second =
  match dir with
  | Horizontal ->
    (match first, second with
     | Some ('-' | 'F' | 'L'), Some ('-' | '7' | 'J') -> Pipe
     | _ -> Gap)
  | Vertical ->
    (match first, second with
     | Some ('|' | 'F' | '7'), Some ('|' | 'L' | 'J') -> Pipe
     | _ -> Gap)
;;

let pad ~dir first second =
  match dir with
  | Horizontal ->
    (match first, second with
     | Some ('-' | 'F' | 'L'), Some ('-' | '7' | 'J') -> [ Pipe; Pipe; Pipe ]
     | l, r -> [ to_cell l; Gap; to_cell r ])
  | Vertical ->
    (match first, second with
     | Some ('|' | 'F' | '7'), Some ('|' | 'L' | 'J') -> [ Pipe; Pipe; Pipe ]
     | l, r -> [ to_cell l; Gap; to_cell r ])
;;

let _ = pad

let intersperce list other =
  let rec intersperce' joined acc =
    match joined with
    | [] -> acc
    | (other, list) :: tl -> intersperce' tl @@ (list :: other :: acc)
  in
  intersperce' (List.zip_exn other @@ List.tl_exn list) [ List.hd_exn list ] |> List.rev
;;

let set_outers map =
  let checked = List.map map ~f:(List.map ~f:(fun _ -> false)) in
  let rec loop map to_mark checked =
    match to_mark with
    | [] -> map
    | (x, y) :: t ->
      let map =
        match get_pos map (x, y) with
        | Empty _ -> set_pos map (x, y) (Empty false)
        | _ -> map
      in
      let to_mark =
        List.filter_map
          [ x + 1, y; x - 1, y; x, y + 1; x, y - 1 ]
          ~f:(fun pos ->
            match pos with
            | x, y when x < 0 || y < 0 -> None
            | x, _ when x >= (List.hd_exn map |> List.length) -> None
            | _, y when y >= List.length map -> None
            | x, y when get_pos checked (x, y) -> None
            | x, y ->
              (match get_pos map (x, y) with
               | Pipe -> None
               | _ -> Some (x, y)))
      in
      let checked =
        List.fold to_mark ~init:checked ~f:(fun acc (x, y) -> set_pos acc (x, y) true)
      in
      loop map (to_mark @ t) checked
  in
  let map = loop map [ 0, 0 ] checked in
  let map =
    loop map [ (List.hd_exn map |> List.length) - 1, List.length map - 1 ] checked
  in
  map
;;

let part2 input =
  let pipes = process_input input in
  List.iter pipes ~f:(fun row ->
    List.iter row ~f:(fun pipe -> printf "%c" pipe);
    print_endline "");
  let s = find_s pipes |> Option.value_exn in
  let s_1, s_2, s_val = gent_s_pipe (fst s) (snd s) pipes in
  let map =
    gen_map_2 (s_1, s_2) (List.hd_exn pipes |> List.length, List.length pipes) s s_val
  in
  let inbetween_horizontal =
    List.map map ~f:(fun line ->
      let line =
        window line ~f:(fun first second -> get_center ~dir:Horizontal first second)
      in
      line)
  in
  let inbetween_vertical =
    window map ~f:(fun first second ->
      List.zip_exn first second
      |> List.map ~f:(fun (l, r) -> get_center ~dir:Vertical l r))
  in
  let inbetween_vertical = List.map inbetween_vertical ~f:(List.intersperse ~sep:Gap) in
  let map = List.map map ~f:(List.map ~f:to_cell) in
  let map =
    List.zip_exn inbetween_horizontal map
    |> List.map ~f:(fun (inbetween, line) -> intersperce line inbetween)
  in
  let map = intersperce map inbetween_vertical in
  let map = set_outers map in
  let amount_inside =
    List.fold map ~init:0 ~f:(fun acc line ->
      List.fold line ~init:acc ~f:(fun acc pipe ->
        match pipe with
        | Empty true -> acc + 1
        | _ -> acc))
  in
  amount_inside
;;

let part_2_test_input =
  {|...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........|}
;;

let () = assert (part2 part_2_test_input = 4)

let part_2_test_input_2 =
  {|.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...|}
;;

let () = assert (part2 part_2_test_input_2 = 8)

let () =
  let input = In_channel.read_all "input.txt" in
  printf "%d\n" (part1 input);
  printf "%d\n" (part2 input)
;;

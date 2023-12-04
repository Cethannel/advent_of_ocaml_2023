open Base
open Stdio

let test_input =
  {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}
;;

let _ = test_input
let rec list_with_len v len = if len = 0 then [] else v :: list_with_len v (len - 1)

let unwrap = function
  | Some x -> x
  | None -> failwith "unwrap failed"
;;

let _ = unwrap

let transform_input lines =
  let rec transform_input' lines acc =
    match lines with
    | [] -> acc
    | line :: lines' -> transform_input' lines' (String.to_list ("." ^ line ^ ".") :: acc)
  in
  let start_acc = [ list_with_len '.' ((List.hd_exn lines |> String.length) + 2) ] in
  (transform_input' lines start_acc |> List.rev) @ start_acc
;;

let get_adjacent_coords x y =
  let diffs = [ -1; 0; 1 ] in
  List.map diffs ~f:(fun dx ->
    List.map diffs ~f:(fun dy ->
      let x' = x + dx in
      let y' = y + dy in
      x', y'))
  |> List.concat
;;

let get_adjacent grid x y =
  let to_check = get_adjacent_coords x y in
  List.map to_check ~f:(fun (x, y) -> List.nth_exn (List.nth_exn grid y) x)
;;

let get_num_from_pos grid x y =
  let row = List.nth_exn grid y in
  let rec get_left x acc =
    if x < 0
    then []
    else (
      match List.nth row x with
      | Some c when Char.is_digit c -> get_left (x - 1) (c :: acc)
      | Some _ -> acc
      | None -> acc)
  in
  let rec get_right x acc =
    if x >= List.length row
    then []
    else (
      match List.nth row x with
      | Some c when Char.is_digit c -> get_right (x + 1) (c :: acc)
      | Some _ -> acc
      | None -> acc)
  in
  let left = get_left x [] in
  let right = get_right (x + 1) [] |> List.rev in
  left @ right |> String.of_char_list |> Int.of_string
;;

let get_num_coords grid =
  let rec collect_num_coords line (x, y) acc =
    let coords = x, y in
    match line with
    | [] -> x, acc
    | c :: line' ->
      if Char.is_digit c
      then collect_num_coords line' (x + 1, y) (coords :: acc)
      else x, acc
  in
  let rec get_num_coords_line line (x, y) acc =
    match line with
    | [] -> acc
    | c :: line' as rest ->
      if Char.is_digit c
      then (
        let x', num = collect_num_coords rest (x, y) [] in
        get_num_coords_line (List.drop rest (x' - x)) (x', y) (num :: acc))
      else get_num_coords_line line' (x + 1, y) acc
  in
  let rec get_num_coords' grid (x, y) acc =
    match grid with
    | [] -> acc
    | line :: grid' ->
      let acc' = get_num_coords_line line (x, y) [] in
      get_num_coords' grid' (x, y + 1) (acc' @ acc)
  in
  get_num_coords' grid (0, 0) []
;;

let is_symbol = function
  | '.' -> false
  | c when Char.is_digit c -> false
  | _ -> true
;;

let is_adjacent grid num ~is_symbol =
  if List.length num > 0
     && List.map num ~f:(fun (x, y) -> get_adjacent grid x y |> List.exists ~f:is_symbol)
        |> List.exists ~f:Fn.id
  then Some (List.hd_exn num)
  else None
;;

let is_gear = function
  | '*' -> true
  | _ -> false
;;

let get_at_coords grid x y = List.nth_exn (List.nth_exn grid y) x

let remove_duplicates grid coords =
  List.dedup_and_sort coords ~compare:(fun (x, y) (x', y') ->
    if y = y'
    then (
      printf "x: %d x': %d\n" x x';
      if abs (x - x') = 2
      then (
        printf "in_between: %c\n" @@ get_at_coords grid (min x x' + 1) y;
        if Char.is_digit @@ get_at_coords grid (min x x' + 1) y
        then (
          printf "removing %d %d\n" x y;
          0)
        else 1)
      else (
        let num = get_num_from_pos grid x y in
        let num' = get_num_from_pos grid x' y' in
        Int.compare num num'))
    else Int.compare y y')
;;

let unit_test_input = {|...489.
66*....
...447.|}

let unit_test = transform_input (String.split_lines unit_test_input)

let gears =
  List.mapi unit_test ~f:(fun y line ->
    List.mapi line ~f:(fun x c -> if is_gear c then Some (x, y) else None))
  |> List.concat
  |> List.filter_opt
;;

let adjacent =
  List.map gears ~f:(fun (x, y) -> get_adjacent_coords x y)
  |> List.filter_map ~f:(fun coords ->
    let coords =
      List.filter coords ~f:(fun (x, y) -> Char.is_digit @@ get_at_coords unit_test x y)
      |> remove_duplicates unit_test
    in
    Some coords)
;;

List.iter adjacent ~f:(fun coords ->
  List.iter coords ~f:(fun (x, y) -> printf "%d %d\n" x y);
  printf "\n")

let nums =
  List.map adjacent ~f:(List.map ~f:(fun (x, y) -> get_num_from_pos unit_test x y))
;;

List.iter nums ~f:(fun nums ->
  List.iter nums ~f:(fun num -> printf "%d " num);
  printf "\n")

let expected = [ [ 2, 1; 4, 1 ]; [ 2, 3; 4, 3 ] ]

let out =
  List.equal
    (fun a b -> List.equal (fun (x, y) (x', y') -> x = x' && y = y') a b)
    adjacent
    expected
;;

let () = assert out

let () =
  let input = In_channel.read_lines "input.txt" in
  (*let input = String.split_lines test_input in*)
  let input = transform_input input in
  let num_coords = get_num_coords input in
  let adjacent =
    List.map num_coords ~f:(is_adjacent ~is_symbol input) |> List.filter_opt
  in
  let nums = List.map adjacent ~f:(fun (x, y) -> get_num_from_pos input x y) in
  let sum = List.fold nums ~init:0 ~f:( + ) in
  printf "Part 1: %d\n" sum;
  let duplicates =
    get_adjacent_coords 4 2
    |> List.filter ~f:(fun (x, y) -> Char.is_digit @@ get_at_coords input x y)
  in
  let out =
    List.dedup_and_sort duplicates ~compare:(fun (x, y) (x', y') ->
      let num = get_num_from_pos input x y in
      let num' = get_num_from_pos input x' y' in
      Int.compare num num')
  in
  List.iter out ~f:(fun (x, y) -> printf "%d %d\n" x y);
  let gears =
    List.mapi input ~f:(fun y line ->
      List.mapi line ~f:(fun x c -> if is_gear c then Some (x, y) else None))
    |> List.concat
    |> List.filter_opt
  in
  let adjacent =
    List.map gears ~f:(fun (x, y) -> get_adjacent_coords x y)
    |> List.filter_map ~f:(fun coords ->
      let coords =
        List.filter coords ~f:(fun (x, y) -> Char.is_digit @@ get_at_coords input x y)
        |> remove_duplicates input
      in
      if List.length coords = 2 then Some coords else None)
  in
  let part_nums =
    List.map adjacent ~f:(fun coords ->
      List.map coords ~f:(fun (x, y) -> get_num_from_pos input x y))
  in
  let ratios = List.map part_nums ~f:(List.fold ~init:1 ~f:( * )) in
  let sum = List.fold ratios ~init:0 ~f:( + ) in
  printf "Part 2: %d\n" sum;
  ()
;;

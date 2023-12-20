open Base
open Stdio

type spring =
  | Working
  | Damaged
  | Unknown

let show_spring spring =
  match spring with
  | Working -> "."
  | Damaged -> "#"
  | Unknown -> "?"
;;

let _ = show_spring Working

let same_springs s1 s2 =
  match s1, s2 with
  | Working, Working -> true
  | Damaged, Damaged -> true
  | Unknown, Unknown -> true
  | _ -> false
;;

let _ = same_springs Working Working

let map_to_string ch =
  match ch with
  | '.' -> Working
  | '#' -> Damaged
  | '?' -> Unknown
  | _ -> failwith "Unknown character"
;;

let get_springs line =
  String.split ~on:' ' line |> List.hd_exn |> String.to_list |> List.map ~f:map_to_string
;;

let get_count line =
  String.split ~on:' ' line
  |> List.last_exn
  |> String.split ~on:','
  |> List.map ~f:Int.of_string
;;

let state_solve input_line count =
  let states =
    [ Working ]
    :: List.map count ~f:(fun i -> List.init i ~f:(fun _ -> Damaged) @ [ Working ])
    |> List.concat
  in
  let start_map = Map.singleton (module Int) 0 1 in
  let out =
    List.fold input_line ~init:start_map ~f:(fun states_dict spring ->
      let dict =
        Map.keys states_dict
        |> List.fold
             ~init:(Map.empty (module Int))
             ~f:(fun new_dict state ->
               match spring with
               | Unknown ->
                 let new_dict =
                   if state + 1 < List.length states
                   then
                     Map.set
                       new_dict
                       ~key:(state + 1)
                       ~data:
                         ((Map.find new_dict (state + 1) |> Option.value ~default:0)
                          + Map.find_exn states_dict state)
                   else new_dict
                 in
                 let new_dict =
                   if same_springs (List.nth_exn states state) Working
                   then
                     Map.set
                       new_dict
                       ~key:state
                       ~data:
                         ((Map.find new_dict state |> Option.value ~default:0)
                          + Map.find_exn states_dict state)
                   else new_dict
                 in
                 new_dict
               | Working ->
                 let new_dict =
                   if state + 1 < List.length states
                      && same_springs (List.nth_exn states (state + 1)) Working
                   then
                     Map.set
                       new_dict
                       ~key:(state + 1)
                       ~data:
                         ((Map.find new_dict (state + 1) |> Option.value ~default:0)
                          + Map.find_exn states_dict state)
                   else new_dict
                 in
                 let new_dict =
                   if same_springs (List.nth_exn states state) Working
                   then
                     Map.set
                       new_dict
                       ~key:state
                       ~data:
                         ((Map.find new_dict state |> Option.value ~default:0)
                          + Map.find_exn states_dict state)
                   else new_dict
                 in
                 new_dict
               | Damaged ->
                 let new_dict =
                   if state + 1 < List.length states
                      && same_springs (List.nth_exn states (state + 1)) Damaged
                   then
                     Map.set
                       new_dict
                       ~key:(state + 1)
                       ~data:
                         ((Map.find new_dict (state + 1) |> Option.value ~default:0)
                          + Map.find_exn states_dict state)
                   else new_dict
                 in
                 new_dict)
      in
      dict)
  in
  let first = Map.find out (List.length states - 1) |> Option.value ~default:0 in
  let second = Map.find out (List.length states - 2) |> Option.value ~default:0 in
  let out = first + second in
  out
;;

let () =
  let example_line = String.to_list ".??..?##?" |> List.map ~f:map_to_string in
  let example_count = [ 1; 3 ] in
  let out = state_solve example_line example_count in
  assert (out = 4)
;;

let part1 input =
  let lines = String.split_lines input in
  let springs = List.map ~f:get_springs lines in
  let counts = List.map ~f:get_count lines in
  let zipped = List.zip_exn springs counts in
  let states = List.map ~f:(fun (spring, count) -> state_solve spring count) zipped in
  List.fold states ~init:0 ~f:( + )
;;

let part2 input =
  let lines = String.split_lines input in
  let springs = List.map ~f:get_springs lines in
  let springs =
    List.map springs ~f:(fun l ->
      l :: List.init 4 ~f:(fun _ -> Unknown :: l) |> List.concat)
  in
  let counts = List.map ~f:get_count lines in
  let counts = List.map counts ~f:(fun l -> List.init 5 ~f:(fun _ -> l) |> List.concat) in
  let zipped = List.zip_exn springs counts in
  let states = List.map ~f:(fun (spring, count) -> state_solve spring count) zipped in
  List.fold states ~init:0 ~f:( + )
;;

let test_input =
  {|???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1|}
;;

let () =
  let out = part1 test_input in
  assert (out = 21);
  let input = In_channel.read_all "input.txt" in
  let out = part1 input in
  assert (out = 7674);
  let out = part2 test_input in
  assert (out = 525152)
;;

let () =
  let input = In_channel.read_all "input.txt" in
  let out = part1 input in
    printf "Out: %d\n" out;
  let out = part2 input in
  printf "Out: %d\n" out;
  assert (out = 4443895258186)
;;

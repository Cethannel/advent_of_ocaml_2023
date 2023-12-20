open Base
open Stdio

type spring =
  | Working
  | Not_working
  | Unknown

let show_spring spring =
  match spring with
  | Working -> "."
  | Not_working -> "#"
  | Unknown -> "?"
;;

let same_springs s1 s2 =
  match s1, s2 with
  | Working, Working -> true
  | Not_working, Not_working -> true
  | Unknown, Unknown -> true
  | _ -> false
;;

let _ = same_springs Working Working

let map_to_string ch =
  match ch with
  | '.' -> Working
  | '#' -> Not_working
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

type perm =
  | Two of spring * spring
  | One of spring

let gen_permutations input_line count =
  ( List.map input_line ~f:(fun v ->
      match v with
      | Unknown -> Two (Working, Not_working)
      | _ -> One v)
    |> List.fold ~init:[] ~f:(fun acc v ->
      match acc with
      | [] -> [ v ]
      | [ One Not_working ] ->
        if List.hd_exn count = 1
        then (
          match v with
          | Two _ ->
            print_endline "Here";
            List.iteri input_line ~f:(fun i v ->
              if i = 0 then print_string " ";
              print_string @@ show_spring v);
            print_endline "";
            acc @ [ One Working ]
          | _ -> acc @ [ v ])
        else acc @ [ v ]
      | _ -> acc @ [ v ])
    |> List.fold ~init:[ [] ] ~f:(fun acc v ->
      List.map acc ~f:(fun l ->
        match v with
        | One s -> [ s :: l ]
        | Two (s1, s2) -> [ s1 :: l; s2 :: l ])
      |> List.concat)
    |> List.map ~f:List.rev
  , count )
;;

let test_out =
  {|.###.##.#...
.###.##..#..
.###.##...#.
.###.##....#
.###..##.#..
.###..##..#.
.###..##...#
.###...##.#.
.###...##..#
.###....##.#|}
;;

let out =
  String.split_lines test_out
  |> List.map ~f:String.to_list
  |> List.map ~f:(List.map ~f:map_to_string)
;;

let _ = out
let thing = String.to_list "#.#" |> List.map ~f:map_to_string

let filter_permutation input_line count =
  List.filter input_line ~f:(fun l ->
    let out, _, _ =
      List.foldi l ~init:(true, count, false) ~f:(fun i acc v ->
        let _ =
          let rest = List.take l 3 in
          let a, _, _ = acc in
          if List.length l < 8 && List.for_all2_exn rest thing ~f:same_springs
          then (
            List.iteri l ~f:(fun j v ->
              if i = j then print_string " ";
              print_string @@ show_spring v);
            print_endline "";
            printf "Count is:";
            let _, b, _ = acc in
            List.iter b ~f:(printf "%d ");
            print_endline "";
            printf "V = %s\n" @@ show_spring v;
            printf "I = %d\n" i;
            printf "Length = %d\n" @@ List.length l;
            printf "A is: %b\n" a;
            true)
          else false
        in
        match v with
        | Working ->
          (match acc with
           | v, [ 0 ], _ when List.length l = i + 1 -> v, [], false
           | _, a, _ when List.length a > 0 && List.length l = i + 1 -> false, [], false
           | v, 0 :: (_ :: _ as t), _ -> v, t, false
           | v, [], _ when List.length l = i + 1 -> v, [], false
           | v, [], _ -> v, [], false
           | _, h :: _, true when h <> 0 -> false, [], false
           | v, acc, _ -> v, acc, false)
        | Not_working ->
          (match acc with
           | false, _, _ -> acc
           | v, [ 1 ], _ -> v, [], false
           | _, a, _ when List.length a > 0 && List.length l = i + 1 -> false, [], false
           | _, [], _ -> false, [], false
           | _, 0 :: _, _ -> false, [], false
           | v, h :: t, _ -> v, (h - 1) :: t, true)
        | Unknown -> failwith "Unknown")
    in
    out)
;;

let part1 input =
  print_endline "";
  let lines = String.split_lines input in
  let springs = List.map ~f:get_springs lines in
  let counts = List.map ~f:get_count lines in
  let zipped = List.zip_exn springs counts in
  let permutations =
    List.map ~f:(fun (spring, count) -> gen_permutations spring count) zipped
  in
  print_endline "Permutations";
  let filtered =
    List.map permutations ~f:(fun (spring, count) -> filter_permutation spring count)
  in
  List.length @@ List.concat filtered
;;

let part2 input =
  print_endline "";
  let lines = String.split_lines input in
  let springs = List.map ~f:get_springs lines in
  let springs =
    List.map springs ~f:(fun l ->
      l :: List.init 4 ~f:(fun _ -> Unknown :: l) |> List.concat)
  in
  let counts = List.map ~f:get_count lines in
  let counts = List.map counts ~f:(fun l -> List.init 5 ~f:(fun _ -> l) |> List.concat) in
  let zipped = List.zip_exn springs counts in
  print_endline "Zipped";
  let i = ref 0 in
  let permutations =
    List.map
      ~f:(fun (spring, count) ->
        printf "Iteration: %d" !i;
        print_endline "";
        i := !i + 1;
        let spring, count = gen_permutations spring count in
        printf "Spring: %d" (List.length spring);
        print_endline "";
        filter_permutation spring count |> List.length)
      zipped
  in
  print_endline "Permutations";
  List.fold permutations ~init:0 ~f:( + )
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
  printf "Out: %d\n" out;
  assert (out = 21);
  let input = In_channel.read_all "input.txt" in
  let out = part1 input in
  assert (out = 7674);
  print_endline "Part 2";
  let out = part2 test_input in
  printf "Out: %d\n" out;
  assert (1 = 2);
  assert (out = 525152)
;;

let () =
  let input = In_channel.read_all "input.txt" in
  (*let out = part1 input in
    printf "Out: %d\n" out;*)
  let out = part2 input in
  printf "Out: %d\n" out
;;

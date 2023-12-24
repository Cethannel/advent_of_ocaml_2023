open Base
open Stdio

let to_ascii c = Char.to_int c
let () = assert (to_ascii 'H' = 72)

let hash s =
  let hash_char current_val c =
    let ascii = to_ascii c in
    let current_val = current_val + ascii in
    let current_val = current_val * 17 in
    current_val % 256
  in
  String.fold s ~init:0 ~f:hash_char
;;

let () = assert (hash "HASH" = 52)
let () = assert (hash "rn" = 0)
let () = assert (hash "rn=1" = 30)
let sanitize s = String.filter s ~f:(fun c -> Char.(c <> '\n'))

let part1 input =
  let input = sanitize input in
  let vals = String.split input ~on:',' in
  let output = List.fold vals ~init:0 ~f:(fun acc v -> acc + hash v) in
  output
;;

type operation =
  | Add of string * string
  | Remove of string

let to_operation s =
  if String.contains s '-'
  then (
    let s = String.split s ~on:'-' in
    Remove (List.hd_exn s))
  else (
    let s = String.split s ~on:'=' in
    Add (List.hd_exn s, List.nth_exn s 1))
;;

let apply_operation ~state = function
  | Add (key, value) ->
    let key' = hash key in
    (match Map.find state key' with
     | None -> Map.add_exn state ~key:key' ~data:[ key, value ]
     | Some values ->
       let new_values =
         if List.exists values ~f:(fun (k, _) -> String.(k = key))
         then
           List.map values ~f:(fun (k, v) -> if String.(k = key) then k, value else k, v)
         else (key, value) :: values
       in
       Map.set state ~key:key' ~data:new_values)
  | Remove key ->
    let key' = hash key in
    (match Map.find state key' with
     | None -> state
     | Some values ->
       let new_values = List.filter values ~f:(fun (k, _) -> String.(k <> key)) in
       Map.set state ~key:key' ~data:new_values)
;;

let part2 input =
  let input = sanitize input in
  let vals = String.split input ~on:',' in
  let input = List.map vals ~f:to_operation in
  let start_state = Map.empty (module Int) in
  let state =
    List.fold input ~init:start_state ~f:(fun state op -> apply_operation ~state op)
  in
  List.fold (Map.to_alist state) ~init:0 ~f:(fun acc (k, v) ->
    (List.rev v
     |> List.foldi ~init:0 ~f:(fun i acc (_, v) ->
       let v = Int.of_string v * (k + 1) * (i + 1) in
       v + acc))
    + acc)
;;

let test_input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

let () =
  let output = part1 test_input in
  assert (output = 1320);
  let output = part2 test_input in
  assert (output = 145)
;;

let () =
  let input = In_channel.read_all "input.txt" in
  let output = part1 input in
  printf "Part1 : %d\n" output;
  let output = part2 input in
  printf "Part2 : %d\n" output
;;

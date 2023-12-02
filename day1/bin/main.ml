module CS = Set.Make (Char)

let test_input = {|
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|}

let _ = test_input

let test_input_2 =
  {|
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
|}
;;

let _ = test_input_2

let test_input_3 =
  {|
one1one
two2two
three3three
four4four
five5five
six6six
seven7seven
eight8eight
nine9nine
|}
;;

let _ = test_input_3

let text_digits =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
;;

let map_text_digits s =
  match s with
  | "one" -> '1'
  | "two" -> '2'
  | "three" -> '3'
  | "four" -> '4'
  | "five" -> '5'
  | "six" -> '6'
  | "seven" -> '7'
  | "eight" -> '8'
  | "nine" -> '9'
  | _ -> failwith "Invalid text digit"
;;

let start_letters =
  List.map (fun s -> String.to_seq s |> List.of_seq |> List.hd) text_digits
  |> List.map CS.singleton
  |> List.fold_left CS.union CS.empty
;;

let end_letters =
  List.map (fun s -> String.to_seq s |> List.of_seq |> List.rev |> List.hd) text_digits
  |> List.map CS.singleton
  |> List.fold_left CS.union CS.empty
;;

let part_1_dig_func h t = if h >= '0' && h <= '9' then t, Some h else t, None

let part_2_dig_func h t ~start_letters ~text_digits ~map_text_digits =
  let l = h :: t in
  let rec collect_text l acc =
    match l with
    | [] -> [], None
    | h :: t ->
      let acc' = h :: acc in
      let rev = List.rev acc' in
      let rev_str = List.map Char.escaped rev |> String.concat "" in
      if List.mem rev_str text_digits then t, Some rev_str else collect_text t acc'
  in
  if h >= '0' && h <= '9'
  then t, Some h
  else if CS.mem h start_letters
  then (
    let t', text = collect_text l [] in
    let t', acc =
      match text with
      | None -> t, None
      | Some text -> t', Some (map_text_digits text)
    in
    t', acc)
  else t, None
;;

let first_part_2_dig_func h t =
  part_2_dig_func h t ~start_letters ~text_digits ~map_text_digits
;;

let _ = first_part_2_dig_func

let last_part_2_dig_func h t =
  part_2_dig_func
    h
    t
    ~start_letters:end_letters
    ~text_digits:
      (List.map
         (fun s ->
           String.to_seq s |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq)
         text_digits)
    ~map_text_digits:(fun s ->
      String.to_seq s
      |> List.of_seq
      |> List.rev
      |> List.to_seq
      |> String.of_seq
      |> map_text_digits)
;;

let _ = last_part_2_dig_func

let get_first l ~dig_func =
  let rec get_first' l =
    match l with
    | [] -> None
    | h :: t ->
      let t', digits = dig_func h t in
      (match digits with
       | None -> get_first' t'
       | Some d -> Some d)
  in
  get_first' l
;;

let get_digits l ~first_fn ~last_fn =
  let first = get_first l ~dig_func:first_fn in
  let last = get_first (List.rev l) ~dig_func:last_fn in
  match first, last with
  | None, None -> None
  | Some f, Some l -> Some [ f; l ]
  | _ -> failwith "Invalid digits"
;;

let to_list s = String.to_seq s |> List.of_seq
let get_first_and_last l = List.nth l 0, List.nth l (List.length l - 1)

let read_file filename =
  let ic = open_in filename in
  let rec read_file' ic acc =
    try
      let line = input_line ic in
      read_file' ic (line :: acc)
    with
    | End_of_file -> List.rev acc
  in
  read_file' ic []
;;

let _ = read_file

let calc input ~first ~last =
  let digits =
    List.filter (fun s -> String.length s > 0) input
    |> List.map to_list
    |> List.map (get_digits ~first_fn:first ~last_fn:last)
    |> List.filter_map (fun l -> l)
  in
  let digits = List.map get_first_and_last digits in
  let nums =
    List.map (fun (a, b) -> Char.escaped a ^ Char.escaped b) digits
    |> List.map int_of_string
  in
  List.fold_left ( + ) 0 nums
;;

let () =
  let input = read_file "input.txt" in
  let part_1 = calc input ~first:part_1_dig_func ~last:part_1_dig_func in
  Printf.printf "Part one: %d\n" part_1;
  let part2 = calc input ~first:first_part_2_dig_func ~last:last_part_2_dig_func in
  Printf.printf "Part two: %d\n" part2
;;

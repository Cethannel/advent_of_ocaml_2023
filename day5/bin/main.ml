open Base
open Stdio

let test_input =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|}
;;

let _ = test_input

let get_seeds input =
  let first_line = String.split_lines input |> List.hd_exn in
  let seeds_string = List.nth_exn (String.split_on_chars first_line ~on:[ ':' ]) 1 in
  let seeds =
    String.split seeds_string ~on:' '
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:Int.of_string
  in
  seeds
;;

type thing_map =
  { from_start : int
  ; to_start : int
  ; amount : int
  }

let gen_map input =
  let lines = String.split_lines input in
  let add_range to_ from acc amount =
    { from_start = from; to_start = to_; amount } :: acc
  in
  let rec loop lines acc =
    match lines with
    | [] -> acc
    | line :: lines ->
      (match String.split line ~on:' ' with
       | [ start; end_; value ] ->
         let start = Int.of_string start in
         let end_ = Int.of_string end_ in
         let value = Int.of_string value in
         let acc = add_range start end_ acc value in
         loop lines acc
       | _ -> loop lines acc)
  in
  loop lines @@ []
;;

let get_map_string name input =
  let lines = String.split_lines input in
  let rec skip_till name lines =
    match lines with
    | [] -> failwith "no map found"
    | line :: lines -> if String.equal line name then lines else skip_till name lines
  in
  let lines = skip_till (name ^ " map:") lines in
  let rec loop lines acc =
    match lines with
    | [] -> acc
    | h :: t -> if String.is_empty h then acc else loop t (h :: acc)
  in
  loop lines []
;;

let get_value map key =
  let rec get_value' map key =
    match map with
    | [] -> key
    | { to_start; from_start; amount } :: t ->
      if key >= from_start && key < from_start + amount
      then to_start + (key - from_start)
      else get_value' t key
  in
  get_value' map key
;;

let get_chunks ~amount list =
  let rec loop list acc =
    match list with
    | [] -> acc
    | _ ->
      let chunk, rest = List.split_n list amount in
      loop rest (chunk :: acc)
  in
  loop list []
;;

let () =
  let test_input = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let chunks = get_chunks ~amount:3 test_input |> List.rev in
  let output = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] in
  assert (List.equal (List.equal Int.equal) chunks output)
;;

let gen_seed_range input =
  let first_line = String.split_lines input |> List.hd_exn in
  let seeds_string = List.nth_exn (String.split_on_chars first_line ~on:[ ':' ]) 1 in
  let seeds =
    String.split seeds_string ~on:' '
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> get_chunks ~amount:2
    |> List.map ~f:(fun list ->
      match list with
      | [ start; amount ] ->
        let out = Int.of_string start, Int.of_string amount in
        out
      | _ -> failwith "invalid seed range")
  in
  seeds
;;

let invert_map map =
  let rec loop map acc =
    match map with
    | [] -> acc
    | { from_start; to_start; amount } :: t ->
      let acc = { from_start = to_start; to_start = from_start; amount } :: acc in
      loop t acc
  in
  loop map []
;;

let get_value_inverse map key = get_value (invert_map map) key
let _ = get_value_inverse

let seeds_contains seeds value =
  List.exists seeds ~f:(fun (start, amount) -> value >= start && value < start + amount)
;;

let _ = seeds_contains

let () =
  let input = test_input in
  let seeds = get_seeds input in
  let seed_to_soil =
    get_map_string "seed-to-soil" input |> List.rev |> String.concat ~sep:"\n" |> gen_map
  in
  let soil_to_fertilizer =
    get_map_string "soil-to-fertilizer" input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let fertilizer_to_water =
    get_map_string "fertilizer-to-water" input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let water_to_light =
    get_map_string "water-to-light" input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let light_to_temperature =
    get_map_string "light-to-temperature" input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let temperature_to_humidity =
    get_map_string "temperature-to-humidity" input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let humidity_to_location =
    get_map_string "humidity-to-location" input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let get_location seed =
    get_value seed_to_soil seed
    |> get_value soil_to_fertilizer
    |> get_value fertilizer_to_water
    |> get_value water_to_light
    |> get_value light_to_temperature
    |> get_value temperature_to_humidity
    |> get_value humidity_to_location
  in
  let locations = List.map seeds ~f:get_location in
  let min = List.min_elt locations ~compare:Int.compare |> Option.value_exn in
  printf "%d\n" min;
  print_endline "Part 2";
  let part_2_seeds = gen_seed_range input in
  print_endline "Getting min";
  let checked_ranges = ref [] in
  let min =
    List.fold part_2_seeds ~init:Int.max_value ~f:(fun acc (start, amount) ->
      print_endline "Getting location";
      let rec loop acc i =
        if i >= amount
        then acc
        else (
          if List.exists !checked_ranges ~f:(fun (start, amount) ->
               i >= start && i < start + amount)
          then print_endline "Already seen";
          let location = get_location (start + i) in
          if location < acc then loop location (i + 1) else loop acc (i + 1))
      in
      let location = loop Int.max_value 0 in
      checked_ranges := (start, amount) :: !checked_ranges;
      if location < acc then location else acc)
  in
  printf "%d\n" min
;;

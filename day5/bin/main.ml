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

let gen_map input =
  let lines = String.split_lines input in
  let rec add_range to_ from map amount =
    if amount < 0
    then map
    else (
      let new_map = Map.set map ~key:from ~data:to_ in
      add_range (to_ + 1) (from + 1) new_map (amount - 1))
  in
  let rec loop lines acc =
    match lines with
    | [] -> acc
    | line :: lines ->
      (match String.split line ~on:' ' with
       | [ start; end_; value ] ->
         printf "start: %s, end: %s, value: %s\n" start end_ value;
         let start = Int.of_string start in
         let end_ = Int.of_string end_ in
         let value = Int.of_string value in
         let acc = add_range start end_ acc value in
         loop lines acc
       | _ -> loop lines acc)
  in
  loop lines @@ Map.empty (module Int)
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
  let a =
    match Map.find map key with
    | Some v -> v
    | None -> key
  in
  a
;;

let () =
  let input = In_channel.read_all "input.txt" in
  let seeds = get_seeds input in
  let seed_to_soil =
    get_map_string "seed-to-soil" test_input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let soil_to_fertilizer =
    get_map_string "soil-to-fertilizer" test_input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let fertilizer_to_water =
    get_map_string "fertilizer-to-water" test_input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let water_to_light =
    get_map_string "water-to-light" test_input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let light_to_temperature =
    get_map_string "light-to-temperature" test_input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let temperature_to_humidity =
    get_map_string "temperature-to-humidity" test_input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let humidity_to_location =
    get_map_string "humidity-to-location" test_input
    |> List.rev
    |> String.concat ~sep:"\n"
    |> gen_map
  in
  let locations =
    List.map seeds ~f:(fun seed ->
      get_value seed_to_soil seed
      |> get_value soil_to_fertilizer
      |> get_value fertilizer_to_water
      |> get_value water_to_light
      |> get_value light_to_temperature
      |> get_value temperature_to_humidity
      |> get_value humidity_to_location)
  in
  let min = List.min_elt locations ~compare:Int.compare |> Option.value_exn in
  printf "%d\n" min
;;

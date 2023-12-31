open Stdio

let () =
  let input = In_channel.read_all "input.txt" in
  (*let result = Day17.part1 input in
  printf "Part1: %d\n" result;
  print_endline "Part2: (takes a while)";*)
  let result = Day17.part2 input in
  printf "Part2: %d\n" result
;;


open Stdio

let () =
  let input = In_channel.read_all "input.txt" in
  let result = Day18.part1 input in
  printf "Part1: %d\n" result;
  let result = Day18.part2 input in
  printf "Part2: %d\n" result

open Stdio
open Day16

let () =
  let input = In_channel.read_all "input.txt" in
  printf "Part1 %d\n" (part1 input);
  print_newline ();
  printf "Part2 %d\n" (part2 input);

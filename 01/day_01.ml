open Core

let rec compute_frequency init_val values =
  match values with
  | [] -> init_val
  | hd::tl -> compute_frequency (init_val + hd) tl
  

let () =
  let filename = "/home/thomas/code/advent_2018/01/input" in
  let lines = In_channel.read_lines filename in
  let values = List.map ~f:(fun line -> Int.of_string line) lines in
  let frequency = compute_frequency 0 values in
  printf "Result %d\n" frequency
  

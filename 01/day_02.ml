open Core

let frequency_already_seen freq seen_freqs =
  let res = List.find ~f:(fun x -> freq = x) seen_freqs in
  match res with
  | None -> false
  | _ -> true

let find_duplicate_frequency values =
  let twice = ref None in
  let last_freq = ref 0 in
  let seen_freqs = ref [] in
  let changes = ref values in 
  while !twice = None  do
    match !changes with
    | [] -> changes := values
    | hd::tl -> let current_freq = !last_freq + hd in
                if frequency_already_seen current_freq !seen_freqs then
                  twice := Some current_freq
                else
                  changes := tl; last_freq := current_freq; seen_freqs := (current_freq::!seen_freqs);
  done;
  twice

let () =
  let filename = "/home/thomas/code/advent_2018/01/input" in
  let lines = In_channel.read_lines filename in
  let values = List.map ~f:(fun line -> Int.of_string line) lines in
  let twice = find_duplicate_frequency values in 
  match !twice with
  | Some res -> printf "First duplicate: %d\n" res
  | None -> printf "Failed"

open Core

(* Taken from https://rosettacode.org/wiki/Levenshtein_distance#OCaml *)
let minimum a b c =
  min a (min b c)
 
let levenshtein_distance s t =
  let m = String.length s
  and n = String.length t in
  (* for all i and j, d.(i).(j) will hold the Levenshtein distance between
     the first i characters of s and the first j characters of t *)
  let d = Array.make_matrix (m+1) (n+1) 0 in
 
  for i = 0 to m do
    d.(i).(0) <- i  (* the distance of any first string to an empty second string *)
  done;
  for j = 0 to n do
    d.(0).(j) <- j  (* the distance of any second string to an empty first string *)
  done;
 
  for j = 1 to n do
    for i = 1 to m do
 
      if s.[i-1] = t.[j-1] then
        d.(i).(j) <- d.(i-1).(j-1)  (* no operation required *)
      else
        d.(i).(j) <- minimum
                       (d.(i-1).(j) + 1)   (* a deletion *)
                       (d.(i).(j-1) + 1)   (* an insertion *)
                       (d.(i-1).(j-1) + 1) (* a substitution *)
    done;
  done;
 
  d.(m).(n)
 
let test s t =
  let levenshtein = levenshtein_distance s t in
  if levenshtein = 1 then
    Printf.printf " %s -> %s\n" s t
  else
    ()

let find_boxes box_ids =
  List.iter box_ids ~f:(fun s -> List.iter box_ids ~f:(fun t -> test s t))

let () =
  let filename = "/home/thomas/code/advent_2018/02/input" in
  let box_ids = In_channel.read_lines filename in
  find_boxes box_ids

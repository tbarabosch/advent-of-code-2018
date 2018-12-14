open Core

(* http://caml.inria.fr/pub/old_caml_site/FAQ/FAQ_EXPERT-eng.html#strings *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let compute_offset c =
  match c with
  | Some a -> (Char.to_int a) - 0x61
  | _ -> -1 (*should never happen, better raise here! *)

let count_letters box_id =
  let chars = explode box_id in
  let counts = Array.init 26 ~f:(fun _ -> 0) in
  for i = 0 to (List.length chars) - 1 do
    let offset = compute_offset (List.nth chars i) in
      counts.(offset) <- counts.(offset) + 1 
  done; counts

let has_count l i =
  match Array.find l ~f:(fun x -> x = i) with
  | Some _ -> 1
  | _ -> 0
                                            

let compute_checksum box_ids =
  let (a, b) = List.fold_left box_ids ~init:(0,0) ~f:(fun (l, r) box_id -> let c = count_letters box_id in
                                                          (l + (has_count c 2), r + (has_count c 3))) in
  a * b

let () =
  let filename = "/home/thomas/code/advent_2018/02/input" in
  let box_ids = In_channel.read_lines filename in
  let checksum = compute_checksum box_ids in
  printf "Checksum: %d\n" checksum

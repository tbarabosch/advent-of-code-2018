open Core

module Key = struct
      module T = struct
        type t = Int.t * Int.t [@@deriving sexp, compare]
        let equal = (=)
        let hash = Hashtbl.hash
      end
      include T
      include Hashable.Make (T)
end

type claim = {id:int; x: int; y:int; h:int; w:int}

exception Parsing_Error of string

let parse_id tokens =
  match List.nth tokens 0  with
  | Some a -> let str_id = String.strip (String.drop_prefix a 1) in
              Int.of_string str_id
  | _ -> raise (Parsing_Error "ID")

let parse_position tokens =
  match List.nth tokens 2 with
  | Some a -> begin 
      match List.nth (String.split ~on:':' a) 0 with
              | Some b -> let pos_x = match List.nth (String.split ~on:',' b) 0 with
                            | Some x -> x
                            | _ -> raise (Parsing_Error "Position") in
                          let pos_y = match List.nth (String.split ~on:',' b) 1 with
                            | Some y -> y
                            | _ -> raise (Parsing_Error "Position") in
                          (Int.of_string pos_x, Int.of_string pos_y)
              | _ -> raise (Parsing_Error "Position")
    end
  | _ -> raise (Parsing_Error "Position")

let parse_dimension tokens =
  match List.nth tokens 3 with
  | Some a -> begin 
              let dim_h = match List.nth (String.split ~on:'x' a) 0 with
                | Some h -> h
                | _ -> raise (Parsing_Error "Dimension") in
              let dim_w = match List.nth (String.split ~on:'x' a) 1 with
                | Some w -> w
                | _ -> raise (Parsing_Error "Dimension") in
              (Int.of_string dim_h, Int.of_string dim_w)
    end
  | _ -> raise (Parsing_Error "Dimension")

let parse_claim raw_claim = 
  let tokens = String.split raw_claim ~on: ' ' in
  let id = parse_id tokens in
  let (x, y) = parse_position tokens in
  let (w, h) = parse_dimension tokens in
  {id; x; y; h; w}

let parse_claims raw_claims =
  List.map ~f:parse_claim raw_claims

let map_claim claim grinch_tbl =
  for w = 0 to claim.w - 1 do
    for h = 0 to claim.h - 1 do
      let x = claim.x + w in
      let y = claim.y + h in
      match Hashtbl.find grinch_tbl (x, y) with
      | Some elem -> Hashtbl.set grinch_tbl ~key:(x, y) ~data:(elem + 1)
      | _ -> Hashtbl.set grinch_tbl ~key:(x, y) ~data:1
    done
  done

let map_areas claims  =
  let grinch_tbl = Key.Table.create () ~size:1000000  in
  List.iter claims ~f:(fun claim -> map_claim claim grinch_tbl); grinch_tbl

let check_claim grinch_tbl claim =
  let res = ref true in
  for w = 0 to claim.w - 1 do
    for h = 0 to claim.h - 1 do
      let x = claim.x + w in
      let y = claim.y + h in
       match Hashtbl.find grinch_tbl (x, y) with
      | Some elem -> if elem > 1 then res := false else ()
      | _ -> printf "SOMETHING IS WRONG!\n"
    done
  done; !res


let find_non_overlapping_claim claims =
  let grinch_tbl = (map_areas claims) in
  List.filter claims ~f:(check_claim grinch_tbl)
  |> List.iter ~f:(fun claim -> printf "ID: %d\n" claim.id)

let () =
  let filename = "/home/thomas/code/advent_2018/03/input" in
  let raw_claims = In_channel.read_lines filename in
  let claims = parse_claims raw_claims in
  find_non_overlapping_claim claims

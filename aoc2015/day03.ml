(* Read file line by line *)
let read_lines filename =
  let file = open_in filename in
  try
    let rec read_all_lines acc =
      try
        let line = input_line file in
        read_all_lines (line :: acc)
      with End_of_file ->
        close_in file;
        List.rev acc
    in
    read_all_lines []
  with e ->
    close_in_noerr file;
    raise e

let lines = read_lines "day03_input"

module PointSet = Set.Make (String)

let rec move x y set chars =
  match chars with
  | [] -> PointSet.cardinal set
  | h :: t ->
      let x, y =
        match h with
        | '^' -> (x, y + 1)
        | '>' -> (x + 1, y)
        | 'v' -> (x, y - 1)
        | '<' -> (x - 1, y)
        | _ -> (x, y)
      in
      let set = PointSet.add (Printf.sprintf "%d,%d" x y) set in
      move x y set t

let rec move' x y lst chars =
  match chars with
  | [] -> lst
  | h :: t ->
      let x, y =
        match h with
        | '^' -> (x, y + 1)
        | '>' -> (x + 1, y)
        | 'v' -> (x, y - 1)
        | '<' -> (x - 1, y)
        | _ -> (x, y)
      in
      let lst = Printf.sprintf "%d,%d" x y :: lst in
      move' x y lst t

let puzzle_01 =
  let set = PointSet.singleton "0,0" in
  List.map
    (fun line -> line |> String.to_seq |> List.of_seq |> move 0 0 set)
    lines
  |> List.fold_left ( + ) 0

let puzzle_01' =
  List.map
    (fun line -> line |> String.to_seq |> List.of_seq |> move' 0 0 [ "0,0" ])
    lines
  |> List.map (fun x ->
         let set = PointSet.of_list x in
         PointSet.cardinal set)
  |> List.fold_left ( + ) 0

let () =
  Printf.printf "Day 03:\npuzzle_01 answer: %d\npuzzle_01' answer: %d\n"
    puzzle_01 puzzle_01'

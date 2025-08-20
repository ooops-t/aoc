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

let rec move_two x y x1 y1 set idx chars =
  match chars with
  | [] -> PointSet.cardinal set
  | h :: t ->
      if idx mod 2 = 0 then
        let x, y =
          match h with
          | '^' -> (x - 1, y)
          | '>' -> (x, y + 1)
          | 'v' -> (x + 1, y)
          | '<' -> (x, y - 1)
          | _ -> (x, y)
        in
        let set = PointSet.add (Printf.sprintf "%d,%d" x y) set in
        move_two x y x1 y1 set (idx + 1) t
      else
        let x1, y1 =
          match h with
          | '^' -> (x1 - 1, y1)
          | '>' -> (x1, y1 + 1)
          | 'v' -> (x1 + 1, y1)
          | '<' -> (x1, y1 - 1)
          | _ -> (x1, y1)
        in
        let set = PointSet.add (Printf.sprintf "%d,%d" x1 y1) set in
        move_two x y x1 y1 set (idx + 1) t

let puzzle_02 =
  let set = PointSet.singleton "0,0" in
  List.map
    (fun line -> line |> String.to_seq |> List.of_seq |> move_two 0 0 0 0 set 1)
    lines
  |> List.fold_left ( + ) 0

let () =
  Printf.printf
    "Day 03:\n\
     puzzle_01 answer: %d\n\
     puzzle_01' answer: %d\n\
     puzzle_02 answer: %d\n"
    puzzle_01 puzzle_01' puzzle_02

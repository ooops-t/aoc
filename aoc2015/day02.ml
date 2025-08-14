(* Read file and return line list *)
let read_input name =
  let file = open_in name in
  let lines = In_channel.input_lines file in
  close_in file;
  lines

let get_size line = Scanf.sscanf line "%dx%dx%d" (fun l w h -> (l, w, h))

(* Sum of the list*)
let sum lst = List.fold_left ( + ) 0 lst

(* Get the list 2 minimum itmes *)
let get_min2 lst = List.sort compare lst |> List.take 2

let puzzle_01 lines =
  List.map
    (fun line ->
      let l, w, h = get_size line in
      let area = (2 * l * w) + (2 * w * h) + (2 * h * l) in
      let feet = get_min2 [ l; w; h ] |> List.fold_left ( * ) 1 in
      area + feet)
    lines

let puzzle_01_answer = read_input "day02_input" |> puzzle_01 |> sum

let puzzle_02 lines =
  List.map
    (fun line ->
      let l, w, h = get_size line in
      let ribbon = get_min2 [ l; w; h ] |> sum in
      (ribbon * 2) + (l * w * h))
    lines

let puzzle_02_answer = read_input "day02_input" |> puzzle_02 |> sum

let () =
  Printf.printf "Day 02:\npuzzle 01 answer: %d\npuzzle 02 answer: %d\n"
    puzzle_01_answer puzzle_02_answer

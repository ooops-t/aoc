let check_char c = match c with '(' -> 1 | ')' -> -1 | _ -> 0

(* Read file contents and covert it to char list *)
let read_input name =
  let file = open_in name in
  let contents = In_channel.input_all file in
  let _ = close_in file in
  let clist = contents |> String.to_seq |> List.of_seq in
  clist

let puzzle_01 contents = List.map check_char contents |> List.fold_left ( + ) 0
let puzzle_01_answer = read_input "day01_input" |> puzzle_01

let puzzle_02 contents =
  let sum = ref 0 in
  List.map check_char contents
  |> List.map (fun x ->
         sum := !sum + x;
         !sum)
  |> List.find_index (fun x -> x = -1)

let puzzle_02_answer =
  let contents = read_input "day01_input" in
  let index = puzzle_02 contents in
  match index with Some x -> x + 1 | None -> 0

let () = Printf.printf "Day 01:\npuzzle 01 answer: %d\npuzzle 02 answer: %d\n" puzzle_01_answer puzzle_02_answer

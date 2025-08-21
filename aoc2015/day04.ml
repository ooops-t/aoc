let md5 str = Digest.string str |> Digest.to_hex
let combine prefix counter = Printf.sprintf "%s%d" prefix counter

let rec find_hash prefix counter =
  let hash = combine prefix counter |> md5 in
  let check = String.starts_with ~prefix:"00000" hash in
  match check with true -> counter | false -> find_hash prefix (counter + 1)

let puzzle_01 = find_hash "ckczppom" 1

let rec find_hash' prefix counter =
  let hash = combine prefix counter |> md5 in
  let check = String.starts_with ~prefix:"000000" hash in
  match check with true -> counter | false -> find_hash' prefix (counter + 1)

let puzzle_02 = find_hash' "ckczppom" 1

let () =
  Printf.printf "Day04: \npuzzle_01 anwser: %d\npuzzle_02 anwser: %d\n"
    puzzle_01 puzzle_02

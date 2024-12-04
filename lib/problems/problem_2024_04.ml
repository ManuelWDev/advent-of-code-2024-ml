let year = 2024
let day = 4

let sliding_window_checker check_pattern arrays =
  let count = ref 0 in
  for i = 0 to Array.length arrays - Array.length check_pattern do
    for j = 0 to Array.length arrays.(0) - Array.length check_pattern.(0) do
      let found = ref true in
      for k = 0 to Array.length check_pattern - 1 do
        for l = 0 to Array.length check_pattern.(0) - 1 do
          match check_pattern.(k).(l) with
          | Some c -> if arrays.(i + k).(j + l) <> c then found := false
          | None -> ()
        done
      done;
      if !found then count := !count + 1
      
    done
  done;
  !count

let part2_pattern = [
  [|
    [|Some 'M'; None; Some 'M'|];
    [|None; Some 'A'; None|];
    [|Some 'S'; None; Some 'S'|];
  |];
  [|
    [|Some 'M'; None; Some 'S'|];
    [|None; Some 'A'; None|];
    [|Some 'M'; None; Some 'S'|];
  |];
  [|
    [|Some 'S'; None; Some 'S'|];
    [|None; Some 'A'; None|];
    [|Some 'M'; None; Some 'M'|];
  |];
  [|
    [|Some 'S'; None; Some 'M'|];
    [|None; Some 'A'; None|];
    [|Some 'S'; None; Some 'M'|];
  |];
]

let part1_pattern = [
  [|
    [|Some 'X'; Some 'M'; Some 'A'; Some 'S'|];
  |];
  [|
    [|Some 'S'; Some 'A'; Some 'M'; Some 'X'|];
  |];
  [|
    [|Some 'X'|];
    [|Some 'M'|];
    [|Some 'A'|];
    [|Some 'S'|];
  |];
  [|
    [|Some 'S'|];
    [|Some 'A'|];
    [|Some 'M'|];
    [|Some 'X'|];
  |];
  [|
    [|Some 'X'; None; None; None|];
    [|None; Some 'M'; None; None|];
    [|None; None; Some 'A'; None|];
    [|None; None; None; Some 'S'|];
  |];
  [|
    [|Some 'S'; None; None; None|];
    [|None; Some 'A'; None; None|];
    [|None; None; Some 'M'; None|];
    [|None; None; None; Some 'X'|];
  |];
  [|
    [|None; None; None; Some 'X'|];
    [|None; None; Some 'M'; None;|];
    [|None; Some 'A'; None;  None|];
    [|Some 'S'; None; None; None|];
  |];
  [|
    [|None; None; None; Some 'S'|];
    [|None; None; Some 'A'; None;|];
    [|None; Some 'M'; None;  None|];
    [|Some 'X'; None; None; None|];
  |];
]

let solve pattern input = 
  let arrays = input 
  |> String.split_on_char '\n'
  |> List.map (fun line -> List.of_seq (String.to_seq line))
  |> List.map Array.of_list |> Array.of_list in
  (pattern |> List.map (fun check_pattern -> sliding_window_checker check_pattern arrays))
  |> List.fold_left (+) 0
  |> string_of_int


module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve part1_pattern input)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve part2_pattern input)
end
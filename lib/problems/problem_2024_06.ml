let year = 2024
let day = 6

type position = {
  x: int;
  y: int;
}

type direction = Left | Right | Up | Down

type instruction = {
  position: position;
  direction: direction;
}

module PosSet = Set.Make(struct
  type t = position
  let compare = compare
end)

module InstructionSet = Set.Make(struct
  type t = instruction
  let compare = compare
end)

let move pos = function
  | Left -> {pos with x = pos.x - 1}
  | Right -> {pos with x = pos.x + 1}
  | Up -> {pos with y = pos.y - 1}
  | Down -> {pos with y = pos.y + 1}

let turn_right = function
  | Left -> Up
  | Up -> Right
  | Right -> Down
  | Down -> Left

let find_start map =
  let start = ref {x=0; y=0} in
  for x = 0 to Array.length map.(0) - 1 do
    for y = 0 to Array.length map - 1 do
      if map.(y).(x) = '^' then start := {x; y}
    done
  done;
  !start

let parse input  =
  let lines = String.split_on_char '\n' input in
  let map = Array.of_list (List.map (fun line -> Array.of_seq (String.to_seq line)) lines) in
  (map, find_start map)

let out_of_bounds map pos =
  pos.y >= Array.length map || pos.y < 0 || pos.x >= Array.length map.(0) || pos.x < 0

let rec count_steps map pos direction =
  let new_pos = move pos direction in
  if out_of_bounds map new_pos then PosSet.empty
  else
    if map.(new_pos.y).(new_pos.x) = '#' then count_steps map pos (turn_right direction)
    else PosSet.add new_pos (count_steps map new_pos direction)

let rec is_loop map visited pos direction =
  let new_pos = move pos direction in
  if out_of_bounds map new_pos then false
  else if InstructionSet.mem {position= new_pos; direction} visited then true
  else if map.(new_pos.y).(new_pos.x) = '#' then is_loop map visited pos (turn_right direction)
  else is_loop map (InstructionSet.add {position= new_pos; direction} visited) new_pos direction

let count_loop_positions map start =
  let count = ref 0 in
  for x = 0 to Array.length map.(0) - 1 do
    for y = 0 to Array.length map - 1 do
      if map.(y).(x) = '.' then (
        map.(y).(x) <- '#';
        if is_loop map (InstructionSet.singleton start) start.position Up then (
          incr count);
        map.(y).(x) <- '.';
        )
    done
  done;
  !count

let part_1 input =
  let (map, position) = parse input in
  count_steps map position Up
  |> PosSet.add position
  |> PosSet.cardinal
  |> string_of_int

  let part_2 input =
    let (map, position) = parse input in
    count_loop_positions map {position; direction=Up}
    |> string_of_int

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (part_1 input)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (part_2 input)
end

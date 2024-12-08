let year = 2024
let day = 8

type vector = {x: int; y: int}
module CharMap = Map.Make(Char)
module VectorSet = Set.Make(struct
  type t = vector
  let compare = compare
end)

let parse input =
  let lines = String.split_on_char '\n' input in
  let map = ref CharMap.empty in
    List.iteri (fun y line ->
      Seq.iteri (fun x c ->
        if c <> '.' then (
          if not (CharMap.mem c !map) then (
            map:= CharMap.add c [] !map);
          map:= CharMap.add c ({x; y}::(CharMap.find c !map)) !map
        )
      ) (String.to_seq line)
    ) lines;
  (!map, {x= String.length (List.hd lines); y= List.length lines})

let rec combinations = function
  | [] -> []
  | [_] -> []
  | x::xs -> List.map (fun y -> (x, y)) xs @ combinations xs

let is_on_map size {x; y} = x >= 0 && y >= 0 && x < size.x && y < size.y

let rec add_all_in_line size start_pos vec set =
  let set = VectorSet.add start_pos set in
  let new_pos = {x = start_pos.x + vec.x; y = start_pos.y + vec.y} in
  if is_on_map size new_pos then
    add_all_in_line size new_pos vec (VectorSet.add new_pos set)
  else set

let add_single size {x; y} vec set =
  let new_pos = {x = x + vec.x; y = y + vec.y} in
  if is_on_map size new_pos then VectorSet.add new_pos set else set

let add_transmitters f acc (p1, p2) =
  let vec = {x = p2.x - p1.x; y = p2.y - p1.y} in 
  acc |> f p2 vec |> f p1 {x= -vec.x; y= -vec.y}

let solve adder input =
  let (map, size) = parse input in
  CharMap.fold (fun _ positions acc -> 
    VectorSet.union (
      combinations positions
      |> List.fold_left (add_transmitters (adder size)) VectorSet.empty
    ) acc
  ) map VectorSet.empty
  |> VectorSet.cardinal
  |> string_of_int

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve add_single input)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve add_all_in_line input)
end
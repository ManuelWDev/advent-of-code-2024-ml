module IntMap = Map.Make(Int)

let year = 2024
let day = 1

let map_tuple f (a, b) = (f a, f b)
let remove_whitespace = List.filter (fun x -> x <> " " && x <> "")

let parse input =
  input 
  |> String.split_on_char '\n'
  |> remove_whitespace
  |> List.map (fun line -> line 
    |> String.split_on_char ' '
    |> remove_whitespace
    |> List.map int_of_string)
  |> List.fold_left (fun (list1, list2) x -> 
    match x with
    | [a; b] -> (a::list1, b::list2)
    | _ -> failwith "Input must have same number of ints left and right"
  ) ([], [])

let rec diferences_sum (list1, list2) =
  match list1, list2 with
  | [], [] -> 0
  | x::xs, y::ys -> abs (x - y) + diferences_sum (xs, ys)
  | _ -> failwith "Lists must have the same length"

let list_to_map list =
  List.fold_left (fun map value -> 
    let count = 
      match IntMap.find_opt value map with
      | Some x -> x + 1
      | None -> 1
    in
    IntMap.add value count map
  ) IntMap.empty list

let similarity_score (map1, map2) =
  IntMap.fold (fun key1 _ acc -> 
    match IntMap.find_opt key1 map2 with
    | Some v2 -> acc + key1 * v2
    | None -> acc + 0
  ) map1 0

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (parse input
    |> map_tuple (List.sort Stdlib.compare)
    |> diferences_sum
    |> string_of_int)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (parse input
    |> map_tuple list_to_map 
    |> similarity_score
    |> string_of_int)
end
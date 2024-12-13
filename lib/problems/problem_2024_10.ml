let year = 2024
let day = 10

module TupleSet = Set.Make(struct
  type t = (int * int)
  let compare = compare
end)

let parse input =
  let lines = String.split_on_char '\n' input in
  Array.of_list (List.map (fun line ->
    Array.of_list (List.init (String.length line) (fun i ->
      Char.code line.[i] - Char.code '0'))
  ) lines)

let count_hiking_trails combiner array_map =
  let rec count_hiking_trails' x y expected_value =
    if x < 0 || x >= Array.length array_map || y < 0 || y >= Array.length array_map.(0) then []
    else (
      let value = array_map.(x).(y) in
      if value = expected_value then (
        if value = 9 then 
          (x, y) :: [] 
        else (
          let new_expected_value = expected_value + 1 in

          array_map.(x).(y) <- 0;
          let result = count_hiking_trails' (x - 1) y new_expected_value
            @ count_hiking_trails' (x + 1) y new_expected_value
            @ count_hiking_trails' x (y - 1) new_expected_value
            @ count_hiking_trails' x (y + 1) new_expected_value in
          array_map.(x).(y) <- value;

          result
        )
      )
      else []
    ) in
    
    let sum = ref 0 in
    for x = 0 to Array.length array_map - 1 do
      for y = 0 to Array.length array_map.(0) - 1 do
        if array_map.(x).(y) = 0 then (
          sum := !sum + (count_hiking_trails' x y 0 |> combiner);
        )
      done
    done;
    !sum

let solve trail_combiner input = input
  |> parse
  |> count_hiking_trails trail_combiner
  |> string_of_int

let part1_combiner l = l
  |> List.fold_left (fun acc elem -> TupleSet.add elem acc) TupleSet.empty
  |> TupleSet.cardinal

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve part1_combiner input)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve List.length input)
end
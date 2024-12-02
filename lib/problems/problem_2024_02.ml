let year = 2024
let day = 2

let rec count_errors comp = function
| [] | [_] -> 0
| x :: y :: rest -> let diff = abs(x - y) in 
                      if diff >= 1 && diff <= 3 && comp x y 
                      then count_errors comp (y :: rest) 
                      else count_errors comp (x :: rest) + 1

let safe_sum max_errors input = input
|> String.split_on_char '\n'
|> List.map (fun line -> line
  |> String.split_on_char ' '
  |> List.map int_of_string
  |> fun l -> ((min (count_errors (<) l) (count_errors (>) l)) <= max_errors ||
               (min (count_errors (<) (List.tl l)) (count_errors (>) (List.tl l)) = max_errors - 1)))
|> List.fold_left (fun sum b -> Bool.to_int b + sum) 0
|> string_of_int

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (safe_sum 0 input)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (safe_sum 1 input)
end
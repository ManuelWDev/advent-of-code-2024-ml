let year = 2024
let day = 7

let rec check_possible operators result = function
  | [] -> false
  | x :: [] -> x = result
  | x :: y :: rest -> List.exists (fun op -> check_possible operators result ((op x y) :: rest)) operators

let solve operators input = input
  |> String.split_on_char '\n'
  |> List.map (fun line -> 
      let parts = Str.split (Str.regexp ": ") line in
      let result = int_of_string (List.nth parts 0) in
      let numbers = List.nth parts 1 |> String.split_on_char ' ' |> List.map int_of_string in
      if check_possible operators result numbers then result else 0
    )
  |> List.fold_left (+) 0
  |> string_of_int

let ( ^|^ ) a b = int_of_string(string_of_int a ^ string_of_int b)

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve [( + ); ( * )] input)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve [( + ); ( * ); ( ^|^ )] input)
end

let year = 2024
let day = 5

module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

let update_rules rules smaller bigger =
  let small_rules = (match IntMap.find_opt smaller rules with
    | Some small_rules -> small_rules
    | None -> IntSet.empty) in
  IntMap.add smaller (IntSet.add bigger small_rules) rules

let parse_rules rule_lines = 
  rule_lines
  |> String.split_on_char '\n'
  |> List.map (fun line -> 
    let parts = String.split_on_char '|' line in
    (int_of_string (List.nth parts 0) , int_of_string (List.nth parts 1))
  )
  |> List.fold_left (fun rules (smaller, bigger) -> update_rules rules smaller bigger) IntMap.empty

let parse_pages pages_lines = 
  pages_lines
  |> String.split_on_char '\n'
  |> List.map (fun line -> line 
    |> String.split_on_char ','
    |> List.map int_of_string
  )

let parse input =
  let rule_lines, pages_lines = 
    let parts = Str.split (Str.regexp "\n\n") input in
      (List.nth parts 0, List.nth parts 1) in
  
  (parse_rules rule_lines, parse_pages pages_lines)

let compare_with_rules rules a b =
  match IntMap.find_opt a rules with
  | Some a_rules -> if IntSet.mem b a_rules then -1 else 1
  | None -> 0

let is_ordered_correctly rules numbers = List.sort (compare_with_rules rules) numbers = numbers

let solve filter calculate_line input =
  let (rules, pages) = parse input in pages
  |> List.filter (filter rules)
  |> List.map (calculate_line rules)
  |> List.fold_left (+) 0
  |> string_of_int

let middle_element l = List.nth l (List.length l / 2)

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve (is_ordered_correctly) (fun _ numbers -> middle_element numbers) input)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve
        (fun rules numbers -> not (is_ordered_correctly rules numbers))
        (fun rules numbers -> 
          let sorted = List.sort (compare_with_rules rules) numbers in
          middle_element sorted
        ) input)
end

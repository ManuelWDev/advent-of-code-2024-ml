let year = 2024
let day = 11

let add k v h = Hashtbl.replace h k (v + (Hashtbl.find_opt h k |> Option.value ~default:0))

let parse input = 
  let table = Hashtbl.create 100 in
  input |> String.split_on_char ' '
  |> List.iter (fun number -> add (int_of_string number) 1 table);
  table

let count_digits number = ceil (log10 (float_of_int (abs number + 1))) |> int_of_float

let split_number n digit_count =
  let split_potency = int_of_float (10. ** float_of_int(digit_count / 2)) in
  let first_part = n / split_potency in
  let second_part = n mod split_potency in
  (first_part, second_part)

let blink src dst =
  Hashtbl.iter (fun key value -> (
    if key = 0 then add 1 value dst
    else (
      let digit_count = count_digits key in
      if digit_count mod 2 = 0 then (
        let (a, b) = split_number key digit_count in
        add a value dst;
        add b value dst;
      )
      else add (key * 2024) value dst
    )
  )) src

let blink_n n src_tbl =
  let dst_tbl = Hashtbl.create (Hashtbl.length src_tbl) in
  let rec loop' n src dst =
    if n = 0 then ()
    else (
      blink src dst;
      Hashtbl.clear src;
      loop' (n - 1) dst src)
  in
  loop' n src_tbl dst_tbl;
  dst_tbl

let solve count input = 
  let table = input |> parse |> blink_n count in
  Hashtbl.fold (fun _ v acc -> acc + v) table 0
  |> string_of_int

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve 25 input)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (solve 75 input)
end
let year = 2024
let day = 3

let base_reg = Re.(seq[
  str "mul(";
  group (rep1 digit);
  char ',';
  group (rep1 digit);
  char ')';
])

let enable_disable_reg = Re.(alt[
  str "do()";
  str "don't()";
  base_reg;
])

let group_mul g = int_of_string (Re.Group.get g 1) * int_of_string (Re.Group.get g 2)

let mul_sum l = l |> List.map group_mul |> List.fold_left (+) 0 

let rec enabled_disabled_sum on = function
  | [] -> 0
  | x :: xs -> match Re.Group.get x 0 with
    | "do()"          -> enabled_disabled_sum true xs
    | "don't()"       -> enabled_disabled_sum false xs
    | _ when on       -> enabled_disabled_sum on xs + group_mul x
    | _               -> enabled_disabled_sum on xs

let base_solve transformer regex input = input 
  |> Re.all (Re.compile regex) |> transformer |> string_of_int 

module Part_1 = struct
  let run (input : string) : (string, string) result = 
    Ok (base_solve mul_sum base_reg input)
end

module Part_2 = struct
  let run (input : string) : (string, string) result = 
    Ok (base_solve (enabled_disabled_sum true) enable_disable_reg input)
end

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

(* 4.1 *)
let check_fermat a b c n =
  match n with
  | n when n <= 2 -> "Enter a number greater than 2"
  | _ ->
    let left = ((pow a n) + (pow b n)) in
    let right = (pow c n) in
    match right with
    | right when right = left -> "Holy smokes, Fermat was wrong!"
    | _ -> "No, that doesn’t work."

let user_fermat =
  print_string "Enter a: ";
  let a = int_of_string(read_line()) in
  print_string "Enter b: ";
  let b = int_of_string(read_line()) in
  print_string "Enter c: ";
  let c = int_of_string(read_line()) in
  print_string "Enter n: ";
  let n = int_of_string(read_line()) in
  check_fermat a b c n

(* 4.2 *)
let is_triangle a b c =
  let pred1 = (c > (a + b)) in
  let pred2 = (b > (a + c)) in
  let pred3 = (a > (c + b)) in
  if (pred1 || pred2 || pred3)
    then "No"
  else "Yes"

let user_tri =
  print_string "Enter a: ";
  let a = int_of_string(read_line()) in
  print_string "Enter b: ";
  let b = int_of_string(read_line()) in
  print_string "Enter c: ";
  let c = int_of_string(read_line()) in
  is_triangle a b c

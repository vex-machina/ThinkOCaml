let add1 = fun x -> x + 1
let sub1 = fun x -> x - 1

let rec fac_aux n acc =
match n with
| 0 -> acc
| _ -> fac_aux (sub1 n) (n * acc)

let factorial n = float_of_int (fac_aux n 1)

(* 6.1 *)
let newt a x =
  ((x +. (a /. x))/. 2.0)

let rec sq_aux a x y =
let e = 1e-7 in
match y with
| y when e > abs_float (y -. x) -> y
| _ -> sq_aux a y (newt a y)

let square_root a x =
  sq_aux a x (newt a x)

(* 6.2 *)
let rec test_square_root m n =
match m with
| m when m = (n +. 1.0) -> ()
| _ -> 
    let estimate = square_root m 3.0 in
    let builtin = sqrt m in
    let diff = abs_float (builtin -. estimate) in
    Printf.printf "%f\t%f\t%f\t%f\n" m estimate builtin diff;
    test_square_root (m +. 1.0) n

(* 6.4 *)
let rec pi_aux k acc =
let e = 1e-15 in
let numer = ((factorial (4 * k)) *. (1103.0 +. (26390.0 *. (float_of_int k)))) in
let denom = ((factorial k) *. (396.0 ** (4.0 *. (float_of_int k)))) in
let term = (numer /. denom) in
  match term with
  | term when term < e -> (((2.0 *. (sqrt 2.0)) /. 9801.0) *. (acc +. term)) ** -1.0
  | _ -> pi_aux (add1 k) (acc +. term)

let estimate_pi =
  pi_aux 0 0.0

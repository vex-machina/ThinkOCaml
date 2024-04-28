let add1 = fun x -> x + 1
let sub1 = fun x -> x - 1

(* 5.2 *)
let rec fib_aux n (x,y) =
  match n with
  | 0 -> x
  | 1 -> y
  | _ -> fib_aux (sub1 n) (y, x + y)

let fibonacci n =
  fib_aux n (0,1)

(* 5.3 *)
let rec ack (m,n) =
match m with
| m when m = 0 -> add1 n
| m when (m > 0) && (n = 0) -> ack ((sub1 m), 1)
| _ -> ack ((sub1 m), ack(m, (sub1 n)))

(* 5.4 *)
let rec rev_aux str acc =
let len = String.length str in
let head = String.make 1 str.[0] in
let last_idx = sub1 len in
let tail = (String.sub str 1 last_idx) in
match len with
| 0 -> acc
| 1 -> (head ^ acc)
| _ -> rev_aux tail (head ^ acc) 

let my_rev str =
  rev_aux str ""

let is_palindrome str =
match str with
| str when str = (my_rev str) -> "Yes"
| _ -> "No"

(* 5.5 *)
let rec is_power a b =
  if (a mod b) = 0
  then
    if a = b
    then true
    else is_power (a / b) b
  else false

(* 5.6 *)
let rec gcd n m =
match m with
| m when m = 0 -> n
| _ ->
  match n with
  | n when n > m -> gcd (n - m) m
  | _ -> gcd n (m - n)

(* 5.7 *)
let rec hof_m n=
match n with
| 0 -> 0
| _ -> n - hof_f (hof_m (sub1 n))
and hof_f n =
match n with
| 0 -> 1
| _ -> n - hof_m (hof_f (sub1 n))

let add1 = fun x -> x + 1
let sub1 = fun x -> x - 1

let print_list to_string l =
    let rec loop rem acc =
        match rem with
        | [] -> acc
        | [s] -> acc ^ (to_string s)
        | (s::ss) ->
            loop ss (acc ^ (to_string s) ^ "; ") in
    print_string "[";
    print_string (loop l "");
    print_endline "]"

let print_str_list = print_list (fun s -> "\"" ^ s ^ "\"")
let print_char_list = print_list (fun c -> "'" ^ String.make 1 c ^ "'")
let print_int_list = print_list string_of_int
let print_float_list = print_list string_of_float

(* 8.3 *)
let rec cum_sum_aux ls cnt acc =
  match ls with
  | ls when ls = [] -> acc
  | _ -> let head = List.hd ls in
         let tail = List.tl ls in
         cum_sum_aux tail (cnt + head) (acc @ [cnt + head])

let cum_sum ls =
  cum_sum_aux ls 0 []

(* 8.4 *)
let rec rem_nth_aux n ls cnt acc =
  match ls with
  | ls when ls = [] -> acc
  | ls when cnt = n -> acc @ (List.tl ls)
  | _ -> rem_nth_aux n (List.tl ls) (add1 cnt) (acc @ [List.hd ls])

let remove_nth n ls =
  rem_nth_aux n ls 0 []

(* 8.5 *)
let rec rem_1st_aux e ls acc =
  match ls with
  | ls when ls = [] -> acc
  | ls when (List.hd ls) = e -> acc @ (List.tl ls)
  | _ -> rem_1st_aux e (List.tl ls) (acc @ [List.hd ls])

let my_rem_1st e ls =
  rem_1st_aux e ls []

let rec rem_aux e ls acc =
  match ls with
  | ls when ls = [] -> acc
  | ls when (List.hd ls) = e -> rem_aux e (List.tl ls) acc
  | _ -> rem_aux e (List.tl ls) (acc @ [List.hd ls])

let my_remove e ls =
  rem_aux e ls []

(* 8.6 *)
let rec is_sort_aux ls prev =
  match ls with
  | ls when ls = [] -> true
  | ls when (List.hd ls) < prev -> false
  | _ -> is_sort_aux (List.tl ls) (List.hd ls)

let is_sorted ls =
  is_sort_aux ls (List.hd ls)

(* 8.7 *)
let rec to_str_aux ls acc =
  match ls with
  | ls when ls = [] -> acc
  | _ -> to_str_aux (List.tl ls) (acc ^ String.make 1 (List.hd ls))

let rec to_str ls =
  to_str_aux ls ""

let last_idx ls =
  sub1 (String.length ls)

let rec to_str_inv_aux str acc =
  match str with
  | str when str = "" -> acc
  | _ -> to_str_inv_aux (String.sub str 1 (last_idx str)) (acc @ [str.[0]])

let to_str_inv str =
  to_str_inv_aux str []

(* 8.8 *)
let xor a b = (a || b) && not (a && b)

let xnor a b = not (xor a b)

let rec is_ele e ls =
  match ls with
  | ls when ls = [] -> false
  | ls when (List.hd ls) = e -> true
  | _ -> is_ele e (List.tl ls)

let rec is_anagram_aux ls1 ls2 =
  let ls1_null = ls1 = [] in
  let ls2_null = ls2 = [] in
  match ls1 with
  | ls1 when (ls1_null || ls2_null) -> xnor ls1_null ls2_null
  | ls1 when is_ele (List.hd ls1) ls2 -> is_anagram_aux (List.tl ls1) (my_rem_1st (List.hd ls1) ls2)
  | _ -> false

let is_anagram str1 str2 =
  is_anagram_aux (to_str_inv str1) (to_str_inv str2)

(* 8.9 *)
let sort_help e1 e2 =
  if e1 > e2
    then 1
  else if e1 = e2
    then 0
  else -1

let sorter ls = List.sort sort_help ls

let rec has_dup_aux ls prev =
  match ls with
  | ls when ls = [] -> false
  | ls when (List.hd ls) = prev -> true
  | _ -> has_dup_aux (List.tl ls) (List.hd ls)

let has_duplicates ls =
  has_dup_aux (sorter ls) []

(* 8.10 *)
let rec rem_dup_aux ls prev acc =
  match ls with
  | ls when ls = [] -> acc
  | ls when (List.hd ls) = prev -> rem_dup_aux (List.tl ls) prev acc
  | _ -> rem_dup_aux (List.tl ls) (List.hd ls) (acc @ List.hd ls)

let remove_dup ls =
  rem_dup_aux (sorter ls) [] []

let add1 = fun x -> x + 1
let sub1 = fun x -> x - 1

(* 7.1 *)
let rec reverse_print s =
  let len = String.length s in
  let last_idx = (sub1 len) in
  print_char s.[last_idx]; print_newline();
  if len = 1
    then ()
  else reverse_print (String.sub s 0 (last_idx));;

(* 7.5 *)
let rec cs_aux c s acc =
  if s = ""
    then acc
  else
    let len = String.length s in
    let head = s.[0] in
    let tail = String.sub s 1 (sub1 len) in
      match head with
      | head when head = c -> cs_aux c tail (add1 acc)
      | _ -> cs_aux c tail acc

let count_str c s =
  cs_aux c s 0

(* 7.7 *)
let alpha_alist =
  [('a', 1); ('b', 2); ('c', 3);
   ('d', 4); ('e', 5); ('f', 6);
   ('g', 7); ('h', 8); ('i', 9);
   ('j', 10); ('k', 11); ('l', 12);
   ('m', 13); ('n', 14); ('o', 15);
   ('p', 16); ('q', 17); ('r', 18);
   ('s', 19); ('t', 20); ('u', 21);
   ('v', 22); ('w', 23); ('x', 24);
   ('y', 25); ('z', 26);]

let rot_char n c =
  let acode = (List.assoc c alpha_alist) in
  let newval = ((acode + n) mod 26) in
  let rev_alpha = List.map (fun (x,y) -> (y,x)) alpha_alist in
  String.make 1 (List.assoc newval rev_alpha)

let rec rot_aux n str acc =
  if str = ""
    then acc
  else
    let len = String.length str in
    let head = str.[0] in
    let tail = String.sub str 1 (sub1 len) in
    rot_aux n tail (acc ^ (rot_char n head))

let rot_str n str =
  rot_aux n str ""

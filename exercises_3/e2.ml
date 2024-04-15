let l = [ 1; 2; 3; 4; 5 ]
let l1 = [ 1; 2; 3; 4; 5 ]
let l2 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

let rec product = function
  | [] -> 1
  | h :: t -> h * product t
;;

let rec concat_str = function
  | [] -> ""
  | h :: t -> h ^ concat_str t
;;

let big_red = function
  | [] -> false
  | h :: _ -> h = "bigred"
;;

let two_or_four = function
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false
;;

let first_two_equal = function
  | f :: s :: _ -> f = s
  | _ -> false
;;

let fifth_elem lst =
  if List.length lst >= 5 then
    List.nth lst 5
  else
    0
;;

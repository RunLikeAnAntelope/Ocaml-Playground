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
    List.nth lst 4
  else
    0
;;

let sort_dec lst = List.sort Stdlib.compare lst |> List.rev
let last_elem lst = List.length lst - 1 |> List.nth lst
let any_zeroes lst = List.exists (fun a -> a = 0) lst

let rec take n lst =
  if n = 0 then
    []
  else (
    match lst with
    | [] -> []
    | h :: t -> h :: take (n - 1) t
  )
;;

let rec tail_take n lst acc =
  if n = 0 then
    acc
  else (
    match lst with
    | [] -> acc
    | h :: t -> tail_take (n - 1) t (h :: acc)
  )
;;

let rec take' n lst = tail_take n lst []

let rec drop n lst =
  if n = 0 then
    lst
  else (
    match lst with
    | [] -> []
    | _ :: t -> drop (n - 1) t
  )
;;

let rec is_decreasing = function
  | [] | [ _ ] -> true
  | h :: (h' :: _ as h't) ->
    if h < h' then
      false
    else
      is_decreasing h't
;;

let rec is_inc_then_dec = function
  | [] | [ _ ] -> true
  | h :: (h' :: _ as h't) ->
    if h' < h then
      is_decreasing h't
    else
      is_inc_then_dec h't
;;

let is_unimodal lst = is_inc_then_dec lst

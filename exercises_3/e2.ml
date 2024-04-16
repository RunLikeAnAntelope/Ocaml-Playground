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

let fifth_elem lst = if List.length lst >= 5 then List.nth lst 4 else 0
let sort_dec lst = List.sort Stdlib.compare lst |> List.rev
let last_elem lst = List.length lst - 1 |> List.nth lst
let any_zeroes lst = List.exists (fun a -> a = 0) lst

let rec take n lst =
  if n = 0
  then []
  else (
    match lst with
    | [] -> []
    | h :: t -> h :: take (n - 1) t)
;;

let rec tail_take n lst acc =
  if n = 0
  then acc
  else (
    match lst with
    | [] -> acc
    | h :: t -> tail_take (n - 1) t (h :: acc))
;;

let rec take' n lst = tail_take n lst []

let rec drop n lst =
  if n = 0
  then lst
  else (
    match lst with
    | [] -> []
    | _ :: t -> drop (n - 1) t)
;;

let rec is_decreasing = function
  | [] | [ _ ] -> true
  | h :: (h' :: _ as h't) -> if h < h' then false else is_decreasing h't
;;

let rec is_inc_then_dec = function
  | [] | [ _ ] -> true
  | h :: (h' :: _ as h't) -> if h' < h then is_decreasing h't else is_inc_then_dec h't
;;

let is_unimodal lst = is_inc_then_dec lst

let rec powerset = function
  | [] -> [ [] ]
  | h :: s ->
    let p = powerset s in
    List.map (List.cons h) p @ p
;;

let rec print_int_list = function
  | [] -> ()
  | h :: t ->
    Printf.printf "%n\n%!" h;
    print_int_list t
;;

let print_int_list' lst = List.iter (fun x -> Printf.printf "%n\n%!" x) lst

type student =
  { first_name : string
  ; last_name : string
  ; gpa : float
  }

let std = { first_name = "Fish"; last_name = "Boy"; gpa = 2.3 }
let name student = student.first_name, student.last_name
let create_student first_name last_name gpa = { first_name; last_name; gpa }

type poketype =
  | Normal
  | Fire
  | Water

type pokemon =
  { name : string
  ; hp : int
  ; ptype : poketype
  }

let charizard = { name = "Charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "Squirtle"; hp = 44; ptype = Water }

let safe_hd = function
  | [] -> None
  | h :: _ -> Some h
;;

let safe_tl = function
  | [] -> None
  | _ :: t -> Some t
;;

let sing_max_hp a b = if a.hp > b.hp then a else b

let rec max_hp = function
  | [] -> None
  | h :: t ->
    (match max_hp t with
     | None -> Some h
     | Some m -> Some (sing_max_hp h m))
;;

type date = int * int * int

let date_before x y =
  let a, b, c = x in
  let a', b', c' = y in
  a < a' || (a = a' && b < b') || (a = a' && b = b' && c < c')
;;

let rec earliest_date = function
  | [] -> None
  | h_date :: t_dates ->
    (match earliest_date t_dates with
     | None -> Some h_date
     | Some m -> Some (if date_before h_date m then h_date else m))
;;

(** [insert k v lst] is an association list that binds key [k] to value [v]
    and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(** [lookup k lst] is [Some v] if association list [lst] binds key [k] to
    value [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t
;;

let map = insert 1 "one" @@ insert 2 "two" @@ insert 3 "three" []
let two = lookup 2 map
let four = lookup 4 map

type suit =
  | Clubs
  | Diamonds
  | Hearts
  | Spades

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card =
  { suit : suit
  ; rank : rank
  }

let ace_of_clubs = { suit = Clubs; rank = Ace }
let queen_of_hearts = { suit = Hearts; rank = Queen }
let two_of_diamonds = { suit = Diamonds; rank = Two }
let seven_of_spades = { suit = Spades; rank = Seven }

(*
   []
   [Some 12; Some 123; None]
   [Some 1; Some 1; None]
   [Some 1]
   []
*)

type quad =
  | I
  | II
  | III
  | IV

type sign =
  | Neg
  | Zero
  | Pos

let sign x = if x = 0 then Zero else if x < 0 then Neg else Pos

let quadrant (x, y) =
  match sign x, sign y with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _, _ -> None
;;

let quadrant_when = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IV
  | _, _ -> None
;;

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec depth = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)
;;

let rec same_shape = function
  | Node (_, leftx, rightx), Node (_, lefty, righty) ->
    same_shape (leftx, lefty) && same_shape (rightx, righty)
  | Leaf, Leaf -> true
  | _ -> false
;;

let list_max = function
  | [] -> raise (Failure "list_max")
  | l -> List.hd @@ List.rev @@ List.sort Stdlib.compare l
;;

let rec str_list_max x = function
  | [] -> x
  | h :: t -> max h (str_list_max x t)
;;

let list_max_string = function
  | [] -> "empty"
  | h :: t -> string_of_int @@ str_list_max h t
;;

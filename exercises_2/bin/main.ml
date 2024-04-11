let () = print_endline "Hello, World!"

let valid_date_fun d m =
  if
    m = "Jan"
    || m = "Mar"
    || m = "May"
    || m = "Jul"
    || m = "Aug"
    || m = "Oct"
    || m = "Dec"
  then
    d >= 1 && d <= 31
  else if m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov" then
    d >= 1 && d <= 30
  else
    m = "Feb" && d >= 1 && d <= 28
;;

let rec fib n =
  if n = 0 then
    0
  else if n = 0 then
    1
  else
    fib (n - 1) + fib (n - 2)
;;

let rec fib_help n pp p =
  if n = 1 then
    p
  else if n = 0 then
    pp
  else
    fib_help (n - 1) p (pp + p)
;;

let fib n = fib_help n 0 1

(*First value where it is negative is 91*)
let rec fib_overflow n =
  if fib n < 0 then
    n
  else
    fib_overflow (n + 1)
;;

let divide ~(numerator : float) ~(denominator : float) = numerator /. denominator
let ( +/. ) x y = (x +. y) /. 2.

open Derivative_parsing_lib
open Derivative_parsing_lib.Derivative
open Derivative_parsing_lib.Derivative.Language
open Derivative_parsing_lib.Ymem
open Derivative_parsing_lib.Yy

let yc f =
  let rec g x = f g x 
  in g

let rec fix f x = f (fix f) x

let fact self = function
  | 0 -> 1
  | x -> x * self (x-1)

let fact_with_fix_point (self: (int -> int) Yfix.self) = function
  | 0 -> 1
  | x -> x * self (x-1)

let _ = (fix fact) 5
let _ = (yc fact) 5

let fact_yy_res = (YY.y fact) 5
let fact_ymem_res = (YMem.y fact) 5
let fact_fix_res = (Yfix.y fact_with_fix_point 0) 5

let language = (
  add "L" (Or(Seq(Ref("L"), Str("x")), Eps)) 
  empty)

let delta_res = (Yfix.y (is_nullable language) false) (find "L" language)

let _ = Printf.printf "delta_res: %b\n%!" delta_res

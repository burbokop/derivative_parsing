

let yc f =
  let rec g x = f g x 
  in g

let rec fix f x = f (fix f) x

let fact self = function
  | 0 -> 1
  | x -> x * self (x-1)

open Derivative_parsing_lib


let fact_with_fix_point (self: (int -> int) Derivative_parsing_lib.Yfix.self) = function
  | 0 -> 1
  | x -> x * self (x-1)


let _ = (fix fact) 5
let _ = (yc fact) 5

let fact_yy_res = (Derivative_parsing_lib.Yy.YY.y fact) 5
let fact_ymem_res = (Derivative_parsing_lib.Ymem.YMem.y fact) 5
let fact_fix_res = (Yfix.y fact_with_fix_point 0) 5

let language = Derivative_parsing_lib.Derivative.(
  Language.add "L" (Or(Seq(Ref("L"), Str("x")), Eps)) 
  Language.empty)

let delta = (Derivative.is_nullable language)

let _ = print_endline "--------------------------------------------------"

let delta_res = (Yfix.y delta false) (Derivative_parsing_lib.Derivative.Language.find "L" language)

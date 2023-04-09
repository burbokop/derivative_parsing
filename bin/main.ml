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
  add "L" (Or(Seq(Ref("L"), Char('x')), Eps)) 
  empty)

let delta = (Yfix.y (is_nullable language) false)
let delta_res =  delta (find "L" language)

let _ = Printf.printf "delta_res: %b\n%!" delta_res



let sss x = Printf.printf "SSS(%d)\n%!" x; x + 1

let _ = Printf.printf "-----\n%!"
let s = lazy (sss 1)
let _ = Printf.printf "+++++\n%!"

let _ = Printf.printf "=====:%d\n%!" (Lazy.force s)


let l2 = (
  add "L" (
    (seq_of_string "foo") <||> 
    (seq_of_string "fqq") <||> 
    (seq_of_string "qaq"))
  empty)

let delta_l2: (rule -> bool) = (Yfix.y (is_nullable l2) false)

let derivative_x = YY.y (derivative delta_l2 l2 'f')
let derivative_x_L = derivative_x (find "L" l2)

let _ = print_endline (Format.asprintf "L:%a -S> %a" Derivative.rule_pp (find "L" l2) Derivative.rule_pp (simplify (find "L" l2)))

let _ = print_endline (Format.asprintf "derivative_x_L:%a -S> %a" Derivative.rule_pp derivative_x_L Derivative.rule_pp (simplify derivative_x_L))

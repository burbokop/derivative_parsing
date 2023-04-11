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
  add "L" (lazy (Or(lazy (Seq(lazy (Ref("L")), lazy (Char('x')))), lazy Eps)))
  empty)

let delta = (Yfix.y (is_nullable language) false)
let delta_res =  delta (find "L" language)

let _ = Printf.printf "delta_res: %b\n%!" delta_res



let sss x = Printf.printf "SSS(%d)\n%!" x; x + 1

let _ = Printf.printf "-----\n%!"
let s = lazy (sss 1)
let _ = Printf.printf "+++++\n%!"

let _ = Printf.printf "=====:%d\n%!" (Lazy.force s)


let derivative_x1 = YMem.y (derivative delta language 'x')
let derivative_x_L1 = derivative_x1 (find "L" language)

let _ = print_endline (Format.asprintf "L1:%a -S> %a" Derivative.rule_ppl (find "L" language) Derivative.rule_ppl (simplify (find "L" language)))

let sd = simplify derivative_x_L1

let _ = print_endline "after simplify"

let _ = print_endline (Format.asprintf "derivative_x_L1:%a -S> %a" Derivative.rule_ppl derivative_x_L1 Derivative.rule_ppl sd)



let l2 = (
  add "L" (
    (seq_of_string "foo") <||> 
    (seq_of_string "fqq") <||> 
    (seq_of_string "qaq"))
  empty)

let delta_l2: (lrule -> bool) = (Yfix.y (is_nullable l2) false)

let derivative_f = YY.y (derivative delta_l2 l2 'f')
let derivative_f_L = derivative_f (find "L" l2)

let _ = print_endline (Format.asprintf "L:%a -S> %a" Derivative.rule_ppl (find "L" l2) Derivative.rule_ppl (simplify (find "L" l2)))

let _ = print_endline (Format.asprintf "derivative_x_L:%a -S> %a" Derivative.rule_ppl derivative_f_L Derivative.rule_ppl (simplify derivative_f_L))



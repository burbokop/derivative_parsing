

open Derivative
open Ymem

type parser = {
	lang: lrule Language.t;
	delta: lrule -> bool;
	der: char -> lrule -> lrule;
}

let parse p begin_key char_list = 
	let rec iter acc = function
		| [] -> p.delta acc
		| h :: t -> iter (p.der h acc) t
	in iter (Language.find begin_key p.lang) char_list


let parse_str p begin_key str = 
	parse p begin_key (List.init (String.length str) (String.get str))


	
let match_str lang begin_key str = 
	let delta = (Yfix.y (is_nullable lang) false) in
  	parse_str {
    	lang = lang;
    	delta = delta;
    	der = (fun x -> YMem.y (derivative delta lang x));
  	} begin_key str

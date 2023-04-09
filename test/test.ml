

open Derivative_parsing_lib
open Derivative_parsing_lib.Derivative

module StrMap = Map.Make(String);;



(* A module with functions to test *)
module To_test = struct
   let lowercase = String.lowercase_ascii
   let capitalize = String.capitalize_ascii
   let str_concat = String.concat ""
   let list_concat = List.append
 end
 
(* The tests *)
let test_is_nullable () =
    Alcotest.(check bool) "is_nullable on recursive language should be `true`" true (let language = Derivative_parsing_lib.Derivative.(
        Language.add "L" (Or(Seq(Ref("L"), Str("x")), Eps)) Language.empty) in
        (Yfix.y (is_nullable language) false) (Language.find "L" language))
  
 (* Run it *)
let () =
    Alcotest.run "Derivative" [
        ("is-nullable-case",
            [
                Alcotest.test_case "Is nullable" `Quick test_is_nullable;
            ]);
    ]
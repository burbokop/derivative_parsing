open Derivative_parsing_lib
open Derivative_parsing_lib.Derivative
open Derivative_parsing_lib.Derivative.Language

(* The tests *)
let test_is_nullable () =
    Alcotest.(check bool) "is_nullable on recursive language should be `true`" true (let language = (
        add "L" (Or(Seq(Ref("L"), Str("x")), Eps)) empty) in
        (Yfix.y (is_nullable language) false) (find "L" language))

(* Run it *)
let () =
    Alcotest.run "Derivative" [
        ("is-nullable-case",
            [
                Alcotest.test_case "Is nullable" `Quick test_is_nullable;
            ]);
    ]

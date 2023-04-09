open Derivative_parsing_lib
open Derivative_parsing_lib.Yy
open Derivative_parsing_lib.Derivative
open Derivative_parsing_lib.Derivative.Language



(* The tests *)
let test_cfg_is_nullable () =
    Alcotest.(check bool) "is_nullable of context-free language" true (let language = (
        add "L" (Or(Seq(Ref("L"), Char('x')), Eps)) empty) in
        (Yfix.y (is_nullable language) false) (find "L" language))

let rl = Alcotest.testable Derivative.rule_pp ( = )

let test_regular_derivative () =
    Alcotest.(check (rl)) "derivative of regular language" (simplify ((seq_of_string "oo") <||> (seq_of_string "qq"))) (
        let language = (
            add "L" (
              (seq_of_string "foo") <||>
              (seq_of_string "fqq") <||>
              (seq_of_string "qaq"))
            empty) in
        let delta = (Yfix.y (is_nullable language) false) in
        let derivative_f = YY.y (derivative delta language 'f') in
        let derivative_f_L = derivative_f (find "L" language) in
        simplify derivative_f_L)

(* Run it *)
let () =
    Alcotest.run "Derivative" [
        ("is-nullable-case",
            [
                Alcotest.test_case "CFG" `Quick test_cfg_is_nullable;
            ]);
        ("derivative-case",
            [
                Alcotest.test_case "Regular" `Quick test_regular_derivative;
            ]);
    ]

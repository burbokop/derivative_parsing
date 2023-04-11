open Derivative_parsing_lib
open Derivative_parsing_lib.Yy
open Derivative_parsing_lib.Ymem
open Derivative_parsing_lib.Derivative
open Derivative_parsing_lib.Derivative.Language



(* The tests *)
let test_cfg_is_nullable () =
    Alcotest.(check bool) "is_nullable of context-free language" true (let language = (
        add "L" (lazy (Or(lazy (Seq(lazy (Ref("L")), lazy (Char('x')))), lazy Eps))) empty) in
        (Yfix.y (is_nullable language) false) (find "L" language))

let rl = Alcotest.testable Derivative.rule_ppl Derivative.eq

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


let test_regular_derivative_memoized () =
    Alcotest.(check (rl)) "derivative of regular language (memoized)" (simplify ((seq_of_string "oo") <||> (seq_of_string "qq"))) (
        let language = (
            add "L" (
              (seq_of_string "foo") <||>
              (seq_of_string "fqq") <||>
              (seq_of_string "qaq"))
            empty) in
        let delta = (Yfix.y (is_nullable language) false) in
        let derivative_f = YMem.y (derivative delta language 'f') in
        let derivative_f_L = derivative_f (find "L" language) in
        simplify derivative_f_L)


let regular1 = (add "L" (
    (seq_of_string "foo") <||>
    (seq_of_string "fqq") <||>
    (seq_of_string "qaq"))
empty)

let test_regular_match_foo () = Alcotest.(check (bool)) "match regular expr (foo)" true (Parse.match_str regular1 "L" "foo")
let test_regular_match_fqq () = Alcotest.(check (bool)) "match regular expr (fqq)" true (Parse.match_str regular1 "L" "fqq")
let test_regular_match_qaq () = Alcotest.(check (bool)) "match regular expr (qaq)" true (Parse.match_str regular1 "L" "qaq")
let test_regular_match_ffo () = Alcotest.(check (bool)) "match regular expr (ffo)" false (Parse.match_str regular1 "L" "ffo")
let test_regular_match_fooo () = Alcotest.(check (bool)) "match regular expr (fooo)" false (Parse.match_str regular1 "L" "fooo")
let test_regular_match_fo () = Alcotest.(check (bool)) "match regular expr (fo)" false (Parse.match_str regular1 "L" "fo")


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
                Alcotest.test_case "Regular (Memoized)" `Quick test_regular_derivative_memoized;                
            ]);
        ("parse-regular-case",
            [
                Alcotest.test_case "foo" `Quick test_regular_match_foo;
                Alcotest.test_case "fqq" `Quick test_regular_match_fqq;
                Alcotest.test_case "qaq" `Quick test_regular_match_qaq;
                Alcotest.test_case "ffo (should not match)" `Quick test_regular_match_ffo;
                Alcotest.test_case "fooo (should not match)" `Quick test_regular_match_fooo;
                Alcotest.test_case "fo (should not match)" `Quick test_regular_match_fo;
            ]);
    ]


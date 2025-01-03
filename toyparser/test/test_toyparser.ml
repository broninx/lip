open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)

let%test "test_eval_2" = parse "5 - 3 - 1" |> eval = Ok 1

let%test "test_eval_3" = parse "3 + 2 * 5" |> eval = Ok 13

let%test "test_eval_4" = parse "1 + 2 / 0" |> eval = Error "Error: tried to divide 2 by zero"

let%test "test_eval_5" = parse "-1 - 2 - -3" |> eval = Ok 0

let%test "test_eval_6" = parse "0x01 + 2" |> eval = Ok 3

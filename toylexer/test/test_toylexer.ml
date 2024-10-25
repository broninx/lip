open Toylexer.Token
open Toylexer.Main


let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)]

(* YOUR TESTS HERE *)

  (*test ATOK*)
let%test "test_ATOK_1" = (lexer_test "1sdr") = [Toylexer.Token.ATOK "1sdr"; Toylexer.Token.EOF]
let%test "test_ATOK_2" = (lexer_test "Rsdr") = [Toylexer.Token.ATOK "Rsdr"; Toylexer.Token.EOF]

(*test BTOK*)
let%test "test_BTOK_1" = (lexer_test "aeiou") = [Toylexer.Token.BTOK "aeiou"; Toylexer.Token.EOF]
let%test "test_ATOK_2" = (lexer_test "ae2uo") = 
  [Toylexer.Token.BTOK "ae";Toylexer.Token.ATOK "2uo"; Toylexer.Token.EOF]

(*test CTOK*)
let%test "test_CTOK_1" = (lexer_test "sdrsdr") = [Toylexer.Token.CTOK "sdrsdr"; Toylexer.Token.EOF]
let%test "test_CTOK_2" = (lexer_test "sdrAsdr") = [Toylexer.Token.CTOK "sdrAsdr"; Toylexer.Token.EOF]
let%test "test_CTOK_3" = (lexer_test "sdrA") = [Toylexer.Token.CTOK "sdrA"; Toylexer.Token.EOF]

(*test DTOK*)
let%test "test_DTOK_1" = (lexer_test "-3.14") = [Toylexer.Token.DTOK "-3.14"; Toylexer.Token.EOF]
let%test "test_DTOK_2" = (lexer_test "-3.") = [Toylexer.Token.DTOK "-3."; Toylexer.Token.EOF]
let%test "test_DTOK_3" = (lexer_test "-.14") = [Toylexer.Token.DTOK "-.14"; Toylexer.Token.EOF]

(*test ETOK*)
let%test "test_ETOK_1" = (lexer_test "0xf22") = [Toylexer.Token.ETOK "0xf22"; Toylexer.Token.EOF]
let%test "test_ETOK_2" = (lexer_test "0Xf2C") = [Toylexer.Token.ETOK "0Xf2C"; Toylexer.Token.EOF]



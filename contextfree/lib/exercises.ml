open Types

(* Use this grammar structure as a blueprint for the exercises. *)
let todo : grammar =
  {
    symbols = [ S ];
    terminals = [ '0'; '1' ];
    productions =
      [
        S --> "0S0";
        S --> "1S1";
        S --> "";
      ];
    start = S;
  }


(* #### Exercise 1, easy (zero_n_one_n) *)
let zero_n_one_n : grammar = 
  {
    symbols = [ S ];
    terminals = ['0';'1'];
    productions = 
      [
        S --> "0S1";
        S --> "" ;
      ];
    start = S;
  }


(* #### Exercise 2, easy (palindromes) *)
let palindromes : grammar = 
  {
    symbols = [S];
    terminals = ['0';'1'];
    productions = 
      [
        S --> "0S0";
        S --> "1S1";
        S --> "0";
        S --> "1";
        S --> "";
      ];
    start = S;
  }

(* #### Exercise 3, medium (balanced_parentheses)*)
let balanced_parentheses : grammar = 
  {
    symbols = [S];
    terminals = ['(';')';'[';']';'{';'}'];
    productions = 
      [
        S --> "SS";
        S --> "(S)";
        S --> "[S]";
        S --> "{S}";
        S --> "";
      ];
      start = S;
  }


(* #### Exercise 4, hard (same_amount)

   Hint 1: you can use 'a' and 'b' for terminals.
   Hint 2: think of the language of words where the number of 0s is one greater
   than the number of 1s and viceversa, then combine them.
*)
let same_amount : grammar = 
  {
    symbols = [S];
    terminals = ['0';'1'];
    productions = 
      [
        S --> "SS";  (**)
        S --> "S01"; (*with this production we have words created by every combination of 01*)
        S --> "S10"; (*now we can create every word with "01" and "10" combined togheter*)
        S --> "S00S11"; (*now we can even create the part of combination with many 0 on the left side and 1 on the other side*)
        S --> "S11S00"; (*and now we have a grammar with every possible word with 0 and 1 linked by recurrence*)
        S --> ""; (**)
      ];
      start = S;
  }

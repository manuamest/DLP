
open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;


let remove_newlines str =
  let lines = String.split_on_char '\n' str in
  String.concat " " lines
;;


let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx,tctx) =
    print_string ">> ";
    flush stdout;
    let rec read_until_double_semicolon acc =
      match input_char stdin with
      | ';' ->
        (match input_char stdin with
        | ';' -> acc
        | c -> read_until_double_semicolon (acc ^ ";;" ^ String.make 1 c)
        )
      | c -> read_until_double_semicolon (acc ^ String.make 1 c)
    in
    try
      let line = remove_newlines(read_until_double_semicolon "") in
      let tm = s token (from_string (line)) in
      loop (execute (vctx,tctx) tm)
    with
      Lexical_error ->
        print_endline "lexical error";
        loop (vctx,tctx)
      | Parse_error ->
        print_endline "syntax error";
        loop (vctx,tctx)
      | Type_error e ->
        print_endline ("type error: " ^ e);
        loop (vctx,tctx) 
      | End_of_file ->
        print_endline "...bye!!!"
  in
  loop (emptyctx,emptyctx)
;;

top_level_loop ()
;;

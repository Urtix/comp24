open MEML_lib
open Ast
open Anf
open Parser

let parse_expr_from_string (s : string) : expression =
  match start_parsing parse_expression s with
  | Ok expr -> expr
  | Error err -> failwith ("Parsing error: " ^ err)
;;

let () =
  let input = Sys.argv.(1) in
  let ast = parse_expr_from_string input in
  let anf_ast = anf_expr ast in
  Format.printf "Original AST: %a\n" pp_expression ast;
  Format.printf "ANF AST: %a\n" pp_expression anf_ast

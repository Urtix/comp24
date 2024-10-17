(** Copyright 2024-2025, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Base

(** Start parse func *)

let start_parsing parser string = parse_string ~consume:All parser string

(* Base *)

let is_char = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_bchar = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_keyword = function
  | "let"
  | "rec"
  | "fun"
  | "if"
  | "then"
  | "else"
  | "true"
  | "false"
  | "match"
  | "with"
  | "val"
  | "in" -> true
  | _ -> false
;;

let is_type = function
  | "int" | "bool" | "string" -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' | '\n' | '\t' | '\r' -> true
  | _ -> false
;;

let is_underscore = function
  | c -> Char.equal c '_'
;;

(* S1mple parsers *)

let parse_white_space = take_while is_whitespace
let parse_white_space1 = take_while1 is_whitespace
let parse_token s = parse_white_space *> s
let parse_token1 s = parse_white_space1 *> s
let pstrtoken s = parse_white_space *> string s
let pstrtoken1 s = parse_white_space1 *> string s
let parens p = pstrtoken "(" *> p <* pstrtoken ")"

let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(** Const parsers *)

let parse_bool =
  parse_white_space
  *> ((fun _ -> CBool true)
      <$> string "true"
      <|> ((fun _ -> CBool false) <$> string "false"))
;;

let parse_int =
  let ps = parse_token (option "" (pstrtoken "-" <|> pstrtoken "+")) in
  let pd = take_while1 is_digit in
  lift2 (fun sign digit -> CInt (Int.of_string @@ sign ^ digit)) ps pd
;;

let parse_str =
  char '"' *> take_while (fun a -> not (phys_equal a '"'))
  <* char '"'
  >>| fun a -> CString a
;;

(* Var parsers *)

let parse_type =
  parse_white_space
  *> char ':'
  *> parse_white_space
  *> ((fun _ -> TInt)
      <$> string "int"
      <|> ((fun _ -> TBool) <$> string "bool")
      <|> ((fun _ -> TString) <$> string "string"))
;;

let check_var cond =
  parse_white_space *> take_while1 cond
  >>= fun v ->
  if is_keyword v
  then fail ("You can not use \"" ^ v ^ "\" keywords as vars")
  else if Char.is_digit @@ String.get v 0
  then fail "Identifier first symbol is letter, not digit"
  else return v
;;

let parse_var =
  let is_entry = function
    | c -> is_char c || is_underscore c || is_digit c
  in
  check_var is_entry
;;

(** Pattern parsers *)

let parse_pattern_var =
  (fun a -> PVar (a, TUnknown))
  <$> parse_var
  <|> parens @@ lift2 (fun a b -> PVar (a, b)) parse_var parse_type
  <|> lift2 (fun a b -> PVar (a, b)) parse_var parse_type
;;

let parse_pconst = (fun v -> PConst v) <$> choice [ parse_int; parse_bool; parse_str ]
let parse_wild = (fun _ -> PWild) <$> pstrtoken "_"
let parse_pattern = parse_wild <|> parse_pconst <|> parse_pattern_var

(** Expression type *)

(* EConst *)

let parse_econst = (fun v -> EConst v) <$> choice [ parse_int; parse_bool; parse_str ]

(* EVar *)

let parse_evar =
  (fun a -> EVar (a, TUnknown))
  <$> parse_var
  <|> parens @@ lift2 (fun a b -> EVar (a, b)) parse_var parse_type
  <|> lift2 (fun a b -> EVar (a, b)) parse_var parse_type
;;

(* EBinaryOp *)

let parse_op char_op op =
  pstrtoken char_op *> return (fun e1 e2 -> EBinaryOp (op, e1, e2))
;;

let pmulti = parse_op "*" Mul <|> parse_op "/" Div <|> parse_op "%" Mod
let padd = parse_op "+" Add <|> parse_op "-" Sub

let pcomp =
  parse_op ">=" Greq <|> parse_op ">" Gre <|> parse_op "<=" Leq <|> parse_op "<" Less
;;

let peq = parse_op "=" Eq <|> parse_op "<>" Neq
let pconj = parse_op "&&" And
let pdisj = parse_op "||" Or

let parse_binop x =
  let multi = chainl1 x pmulti in
  let add = chainl1 multi padd in
  let comp = chainl1 add pcomp in
  let eq = chainl1 comp peq in
  let conj = chainl1 eq pconj in
  chainl1 conj pdisj <* parse_white_space
;;

let parse_ebinop = parse_binop @@ (parse_econst <|> parse_evar)

(* EApp *)

(* EIfElse *)

let parse_eifelse =
  fix
  @@ fun _ ->
  lift3
    (fun e1 e2 e3 -> EIfElse (e1, e2, e3))
    (pstrtoken "if" *> parse_ebinop)
    (pstrtoken "then" *> parse_ebinop)
    (pstrtoken "else" *> parse_ebinop)
;;

(* ELetIn *)

let parse_rec =
  parse_white_space *> pstrtoken "let" *> option "false" (pstrtoken1 "rec")
  >>| fun x -> if String.( <> ) x "false" then Rec else Notrec
;;

(* EFun *)

(* Expression parsers *)

(** Binding type *)
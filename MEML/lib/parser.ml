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
  | "in" -> true
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
  char '"' *> take_while (fun a -> not (phys_equal a '"')) <* char '"' >>| fun a -> CString a
;;

let parse_const = choice [ parse_int; parse_bool; parse_str ]


let var cond =
  parse_white_space *> take_while1 cond
  >>= fun v ->
  if (phys_equal (String.length v) 0)
  then fail "Not identifier"
  else if is_keyword v
  then fail ("You can not use" ^ v ^ "keywords as vars")
  else if Char.is_digit @@ String.get v 0
  then fail "Identifier first simbol is letter, not digit"
  else return v
;;

let p_var =
  let is_entry = function
    | c -> is_char c || is_underscore c || is_digit c
  in
  var is_entry
;;

let parse_Const = (fun v -> PConst v) <$> parse_const
let parse_var = (fun v -> PVar (v, TUnknown)) <$> p_var
let parse_wild = (fun _ -> PWild) <$> pstrtoken "_"


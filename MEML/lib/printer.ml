open Ast

let constant_printer formatter = function
  | CInt i -> Format.fprintf formatter "%d" i
  | CBool false -> Format.fprintf formatter "false"
  | CBool true -> Format.fprintf formatter "true"
  | CNil -> Format.fprintf formatter "[]"
;;

let tuple_printer printer formatter l =
  List.iteri
    (fun i e ->
      if i <> 0 then Format.fprintf formatter ", " else ();
      printer formatter e)
    l
;;

let rec pattern_printer formatter = function
  | PWild -> Format.fprintf formatter "_"
  | PCon (hd, tl) ->
    Format.fprintf formatter "(%a :: %a)" pattern_printer hd pattern_printer tl
  | PVar (n, _) -> Format.fprintf formatter "%s" n
  | PTuple t -> Format.fprintf formatter "(%a)" (tuple_printer pattern_printer) t
  | PConst c -> constant_printer formatter c
;;

let rec_printer formatter = function
  | Rec -> Format.fprintf formatter "rec"
  | Notrec -> ()
;;

let binop_printer formatter = function
  | Add -> Format.fprintf formatter "+"
  | Sub -> Format.fprintf formatter "-"
  | Mul -> Format.fprintf formatter "*"
  | Div -> Format.fprintf formatter "/"
  | And -> Format.fprintf formatter "&"
  | Or -> Format.fprintf formatter "||"
  | Eq -> Format.fprintf formatter "="
  | Neq -> Format.fprintf formatter "<>"
  | Less -> Format.fprintf formatter "<"
  | Gre -> Format.fprintf formatter ">"
  | Leq -> Format.fprintf formatter "<="
  | Greq -> Format.fprintf formatter ">="
;;

let rec expression_printer formatter e =
  match e with
  | EConst c -> constant_printer formatter c
  | EVar (n, _) -> Format.fprintf formatter "%s" n
  | EFun (p, e) ->
    Format.fprintf formatter "(fun %a -> %a)" pattern_printer p expression_printer e
  | EApp (e1, e2) ->
    Format.fprintf formatter "%a %a" expression_printer e1 expression_printer e2
  | EIfElse (i, t, e) ->
    Format.fprintf
      formatter
      "\n  if %a\n  then %a\n  else %a"
      expression_printer
      i
      expression_printer
      t
      expression_printer
      e
  | ELetIn (r, n, e, ine) ->
    Format.fprintf
      formatter
      "\n  let %a %s = %a\n  in %a"
      rec_printer
      r
      n
      expression_printer
      e
      expression_printer
      ine
  | ETuple t -> Format.fprintf formatter "(%a)" (tuple_printer expression_printer) t
  | EBinaryOp (op, l, r) ->
    Format.fprintf
      formatter
      "%a %a %a"
      expression_printer
      l
      binop_printer
      op
      expression_printer
      r
  | _ -> failwith "asf"
;;

let bindings_printer formatter bindings =
  List.iter
    (fun bind ->
      (match bind with
       | Let (r, n, e) ->
         Format.fprintf
           formatter
           "let %a %s = %a"
           rec_printer
           r
           n
           expression_printer
           e
       | Expression e -> expression_printer formatter e);
      Format.fprintf formatter "\n")
    bindings
;;

let printer bindings = Format.asprintf "%a" bindings_printer bindings

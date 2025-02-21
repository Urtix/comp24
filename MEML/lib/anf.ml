open Ast

let counter = ref 0
let gen_tmp_var () =
  let var_name = Printf.sprintf "tmp_%d" !counter in
  counter := !counter + 1;
  var_name

let rec anf_expr (expr : expression) : expression =
    match expr with
    | EConst _ -> expr
    | EVar _ -> expr
    | EBinaryOp (op, e1, e2) ->
        let tmp1 = gen_tmp_var () in
        let tmp2 = gen_tmp_var () in
        let e1_anf = anf_expr e1 in
        let e2_anf = anf_expr e2 in
        ELetIn (Notrec, tmp1, e1_anf,
          ELetIn (Notrec, tmp2, e2_anf,
            EBinaryOp (op, EVar (tmp1, TUnknown), EVar (tmp2, TUnknown))
          )
        )
    | EApp (e1, e2) ->
        let tmp1 = gen_tmp_var () in
        let tmp2 = gen_tmp_var () in
        let e1_anf = anf_expr e1 in
        let e2_anf = anf_expr e2 in
        ELetIn (Notrec, tmp1, e1_anf,
          ELetIn (Notrec, tmp2, e2_anf,
            EApp (EVar (tmp1, TUnknown), EVar (tmp2, TUnknown))
          )
        )
    | EIfElse (cond, e1, e2) ->
        let tmp_cond = gen_tmp_var () in
        let tmp1 = gen_tmp_var () in
        let tmp2 = gen_tmp_var () in
        let cond_anf = anf_expr cond in
        let e1_anf = anf_expr e1 in
        let e2_anf = anf_expr e2 in
        ELetIn (Notrec, tmp_cond, cond_anf,
          EIfElse (EVar (tmp_cond, TUnknown),
            ELetIn (Notrec, tmp1, e1_anf, EVar (tmp1, TUnknown)),
            ELetIn (Notrec, tmp2, e2_anf, EVar (tmp2, TUnknown))
          )
        )
    | ELetIn (rec_flag, name, e1, e2) ->
        let e1_anf = anf_expr e1 in
        let e2_anf = anf_expr e2 in
        ELetIn (rec_flag, name, e1_anf, e2_anf)
    | EFun (pat, e) ->
        let e_anf = anf_expr e in
        EFun (pat, e_anf)
    | EList (e1, e2) ->
        let tmp1 = gen_tmp_var () in
        let tmp2 = gen_tmp_var () in
        let e1_anf = anf_expr e1 in
        let e2_anf = anf_expr e2 in
        ELetIn (Notrec, tmp1, e1_anf,
          ELetIn (Notrec, tmp2, e2_anf,
            EList (EVar (tmp1, TUnknown), EVar (tmp2, TUnknown))
          )
        )
    | ETuple es ->
        let tmp_vars = List.map (fun _ -> gen_tmp_var ()) es in
        let es_anf = List.map anf_expr es in
        List.fold_right2
          (fun var e acc -> ELetIn (Notrec, var, e, acc))
          tmp_vars es_anf
          (ETuple (List.map (fun var -> EVar (var, TUnknown)) tmp_vars))
    | EMatch (e, cases) ->
        let tmp_e = gen_tmp_var () in
        let e_anf = anf_expr e in
        let cases_anf = List.map (fun (pat, e) -> (pat, anf_expr e)) cases in
        ELetIn (Notrec, tmp_e, e_anf, EMatch (EVar (tmp_e, TUnknown), cases_anf))

let anf_bindings (bindings : bindings) : bindings =
  match bindings with
  | Let (rec_flag, name, expr) ->
      Let (rec_flag, name, anf_expr expr)
  | Expression expr ->
      Expression (anf_expr expr)

let anf_statements (statements : statements) : statements =
  List.map anf_bindings statements

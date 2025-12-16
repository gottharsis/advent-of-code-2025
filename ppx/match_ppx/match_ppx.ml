open Ppxlib

(* --- let%match  -------------------------------------------------------- *)
(* Expands:
     let%match pat = expr in body

   into:

     let tmp = expr in
     match tmp with
     | pat -> body
     | _   -> failwith "non-exhaustive let%match"
*)

let expand_let_match ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let module B = (val Ast_builder.make loc : Ast_builder.S) in
  match expr.pexp_desc with
  | Pexp_let (Nonrecursive, [ vb ], body) ->
      let pat = vb.pvb_pat in
      let rhs = vb.pvb_expr in

      let tmp = gen_symbol ~prefix:"tmp" () in
      let ptmp = B.pvar tmp in
      let etmp = B.evar tmp in

      [%expr
        let [%p ptmp] = [%e rhs] in
        match [%e etmp] with
        | [%p pat] -> [%e body]
        | _ -> failwith "non-exhaustive let%%match"]
  | _ ->
      Location.raise_errorf ~loc
        "let%%match expects: let%%match pat = expr in body"

(* --- let%match_res  ---------------------------------------------------- *)
(* Expands:
     let%match_res pat = expr in body

   into:

     let tmp = expr in
     match tmp with
     | pat   -> Ok body
     | value -> Error value
*)

let expand_let_match_res ~ctxt expr =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let module B = (val Ast_builder.make loc : Ast_builder.S) in
  match expr.pexp_desc with
  | Pexp_let (Nonrecursive, [ vb ], body) ->
      let pat = vb.pvb_pat in
      let rhs = vb.pvb_expr in

      let tmp = gen_symbol ~prefix:"tmp" () in
      let ptmp = B.pvar tmp in
      let etmp = B.evar tmp in

      let failv = gen_symbol ~prefix:"value" () in
      let pfailv = B.pvar failv in
      let efailv = B.evar failv in

      [%expr
        let [%p ptmp] = [%e rhs] in
        match [%e etmp] with
        | [%p pat] -> Ok [%e body]
        | [%p pfailv] -> Error [%e efailv]]
  | _ ->
      Location.raise_errorf ~loc
        "let%%match_res expects: let%%match_res pat = expr in body"

(* --- register both extensions ------------------------------------------ *)

let ext_match =
  Extension.V3.declare "match" (* let%match *) Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_let_match

let ext_match_res =
  Extension.V3.declare "match_res" (* let%match_res *)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand_let_match_res

let () =
  Driver.register_transformation
    ~rules:
      [
        Context_free.Rule.extension ext_match;
        Context_free.Rule.extension ext_match_res;
      ]
    "match_ppx"

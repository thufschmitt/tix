module P = Simple.Ast
module T = Typed_ast
module E = Typing_env
module L = Parse.Location
module TE = Types.Environment

module Pattern = Typecheck_pat

exception TypeError of L.t * string

let () = Printexc.register_printer @@ function
  | TypeError (loc, msg) -> CCOpt.pure @@
    (Format.fprintf Format.str_formatter "TypeError at %a: %s"
       L.pp loc
       msg;
     Format.flush_str_formatter ())
  | _ -> None

let typeError loc e = Format.ksprintf (fun s -> raise (TypeError (loc, s))) e

module Bindings = struct
  let explicit_annotations (tenv : TE.t) (bindings : P.binding list)
    : (T.binding_lhs * P.expr) list * E.t =
    let half_typed_bindings = List.map (fun ((var, maybe_annot), e) ->
        match maybe_annot with
        | Some annot ->
          CCOpt.map
            (fun t -> (T.Bannotated (var, t), e))
            (Annotations.to_type tenv annot)
          |> CCOpt.get_lazy (fun () -> assert false)
        | None -> (T.BnonAnnotated var, e)
      )
        bindings
    in
    let new_env =
      List.fold_left
        (fun accu (lhs, _) ->
           match lhs with
           | T.Bannotated (x, t) -> E.add x t accu
           | T.BnonAnnotated x -> E.add x Types.Builtins.grad accu)
        E.empty
        half_typed_bindings
    in
    half_typed_bindings, new_env

  let report_inference_results (typed_binds : T.bindings) : E.t =
    List.fold_left
      (fun accu (lhs, rhs) ->
         match lhs with
         | T.Bannotated (x, t) -> E.add x t accu
         | T.BnonAnnotated x ->
           E.add x (T.get_typ rhs) accu)
      E.empty
      typed_binds
end

let typeof_const = function
  | P.Cbool true -> Types.Builtins.true_type
  | P.Cbool false -> Types.Builtins.false_type
  | P.Cint  i ->
    Types.Builtins.interval
      (Types.Intervals.singleton_of_int i)
  | P.Cnil -> Types.Builtins.nil
  | P.Cstring _  -> assert false

let () = ()
(* For some reason, ocp-indent don't work correctly if we don't put this here *)

module rec Infer : sig
  val expr : TE.t -> E.t -> P.expr -> T.expr
end = struct
  let rec expr (tenv : TE.t) (env : E.t) : P.expr -> T.expr = fun e ->
    e |> L.With_loc.map @@ function
    | P.Econstant c ->
      let typ = typeof_const c in
      T.With_type.make ~description:(T.Econstant c) ~typ
    | P.Evar v ->
      begin match E.lookup env v with
        | Some t -> T.With_type.make ~description:(T.Evar v) ~typ:t
        | None -> typeError e.L.With_loc.location "Unbount variable %s" v
      end
    | P.Elambda (pat, e) ->
      let (added_env, typed_pat) = Pattern.infer tenv pat in
      let domain = T.get_typ typed_pat in
      let typed_e = expr tenv (E.merge env added_env) e in
      let codomain = T.get_typ typed_e in
      T.With_type.make
        ~description:(T.Elambda (typed_pat, typed_e))
        ~typ:(Types.Builtins.arrow domain codomain)
    | P.EfunApp (e1, e2) ->
      let typed_e1 = expr tenv env e1
      and typed_e2 = expr tenv env e2
      in
      let t1 = T.get_typ typed_e1
      and t2 = T.get_typ typed_e2
      in
      if not @@ Types.sub t1 Types.Builtins.(arrow empty any) then
        typeError e.L.With_loc.location
          "This expression has type %s which is not an arrow type. \
           It can't be applied"
          (Types.show t1);
      let t1arrow = Cduce_lib.Types.Arrow.get t1 in
      let dom = Cduce_lib.Types.Arrow.domain t1arrow in
      if Types.sub t2 dom then
        T.With_type.make
          ~description:(T.EfunApp(typed_e1, typed_e2))
          ~typ:(Cduce_lib.Types.Arrow.apply t1arrow t2)
      else
        typeError e.L.With_loc.location "Invalid function application"
    | P.Elet (binds, e) ->
      let module B = Bindings in
      let half_typed_binds, binds_env = B.explicit_annotations tenv binds in
      let typed_binds =
        List.map
          (fun (lhs, rhs) -> (lhs, expr tenv (E.merge env binds_env) rhs))
          half_typed_binds
      in
      let added_env = B.report_inference_results typed_binds in
      let typed_e = expr
          tenv
          (E.merge env added_env)
          e
      in
      T.With_type.make
        ~description:(T.Elet (typed_binds, typed_e))
        ~typ:(T.get_typ typed_e)
    | _ -> (ignore (expr, env); assert false)
end

and Check : sig
  val expr : TE.t -> E.t -> P.expr -> Types.t -> T.expr
end = struct
  let check_subtype loc ~inferred ~expected =
    if not (Types.sub inferred expected) then
      typeError loc
        "The inferred type for this expression is %s while %s was expected"
        (Types.show inferred)
        (Types.show expected)

  let expr _tenv _env e expected_typ =
    let loc = L.With_loc.loc e in
    e |> L.With_loc.map @@ function
    | P.Econstant c ->
      let c_ty = typeof_const c in
      check_subtype loc ~inferred:c_ty ~expected:expected_typ;
      T.With_type.make
        ~description:(T.Econstant c)
        ~typ:expected_typ
    | _ -> assert false
end

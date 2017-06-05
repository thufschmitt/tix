module P = Simple.Ast
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
    : (string * Types.t option * P.expr) list * E.t =
    let half_typed_bindings = List.map (fun ((var, maybe_annot), e) ->
        let type_constraint =
          CCOpt.flat_map
            (fun annot ->
               CCOpt.map
                 (fun t -> Some t)
                 (Annotations.to_type tenv annot)
               |> CCOpt.get_lazy (fun () -> assert false))
            maybe_annot
        in (var, type_constraint, e)
      )
        bindings
    in
    let new_env =
      List.fold_left
        (fun accu (x, constr, _) ->
           E.add x (CCOpt.get_or ~default:Types.Builtins.grad constr) accu)
        E.empty
        half_typed_bindings
    in
    half_typed_bindings, new_env

  let report_inference_results
      (typed_binds : (string * Types.t option * Types.t) list)
    : E.t =
    List.fold_left
      (fun accu (x, constr, rhs_typ) ->
         E.add x (CCOpt.get_or ~default:rhs_typ constr) accu)
      E.empty
      typed_binds
end

let typeof_const = function
  | P.Cbool true -> Types.Builtins.true_type
  | P.Cbool false -> Types.Builtins.false_type
  | P.Cint  i ->
    Types.Builtins.interval
      (Types.Intervals.singleton_of_int i)
  | P.Cstring _  -> assert false

let () = ()
(* For some reason, ocp-indent don't work correctly if we don't put this here *)

module rec Infer : sig
  val expr : TE.t -> E.t -> P.expr -> Types.t
end = struct
  let rec expr (tenv : TE.t) (env : E.t) : P.expr -> Types.t = fun e ->
    L.With_loc.description e |> function
    | P.Econstant c -> typeof_const c
    | P.Evar v ->
      CCOpt.get_lazy
        (fun () -> typeError e.L.With_loc.location "Unbount variable %s" v)
        (E.lookup env v)
    | P.Elambda (pat, e) ->
      let (added_env, domain) = Pattern.infer tenv pat in
      let codomain = expr tenv (E.merge env added_env) e in
      Types.Builtins.arrow domain codomain
    | P.EfunApp (e1, e2) ->
      let t1 = expr tenv env e1
      and t2 = expr tenv env e2
      in
      if not @@ Types.sub t1 Types.Builtins.(arrow empty any) then
        typeError e.L.With_loc.location
          "This expression has type %s which is not an arrow type. \
           It can't be applied"
          (Types.show t1);
      let t1arrow = Cduce_lib.Types.Arrow.get t1 in
      let dom = Cduce_lib.Types.Arrow.domain t1arrow in
      if Types.sub t2 dom then
        Cduce_lib.Types.Arrow.apply t1arrow t2
      else
        typeError e.L.With_loc.location "Invalid function application"
    | P.Elet (binds, e) ->
      Common.let_binding expr tenv env binds e
    | _ -> (ignore (expr, env); assert false)
end

and Check : sig
  val expr : TE.t -> E.t -> P.expr -> Types.t -> unit
end = struct
  let check_subtype loc ~inferred ~expected =
    if not (Types.sub inferred expected) then
      typeError loc
        "The inferred type for this expression is %s while %s was expected"
        (Types.show inferred)
        (Types.show expected)

  (** The \mathscr{A} operator from the paper *)
  let a_op (_, arrow_bdd) =
    let squared_union i_set j_set =
      List.map (fun (si, ti) ->
          List.map (fun (sj, tj) ->
              Types.Builtins.(cap si sj, cup ti tj))
            i_set)
        j_set
      |> List.flatten
    in
    CCList.fold_left squared_union Types.Builtins.[(any, empty)] arrow_bdd

  let rec expr tenv env e expected =
    let loc = L.With_loc.loc e in
    L.With_loc.description e |> function
    | P.Econstant c ->
      let c_ty = typeof_const c in
      check_subtype loc ~inferred:c_ty ~expected
    | P.Evar v ->
      begin match E.lookup env v with
        | Some t -> check_subtype loc ~inferred:t ~expected;
        | None -> typeError e.L.With_loc.location "Unbount variable %s" v
      end
    | P.Elambda (pat, e) ->
      check_subtype loc ~inferred:expected ~expected:Cduce_lib.Types.Arrow.any;
      (* XXX: destruct [expected] with the A(t) function from the paper *)
      let expected_arrow = Cduce_lib.Types.Arrow.get expected in
      List.iter (fun (dom, codom) ->
          let (added_env, _) = Pattern.infer ~t_constr:dom tenv pat in
          let _typed_e = expr tenv (E.merge env added_env) e codom in
          ())
        (a_op expected_arrow)
    | P.Elet (binds, e) ->
      Common.let_binding expr tenv env binds e expected
    | _ -> assert false
end

and Common : sig
  val let_binding : (TE.t -> E.t -> P.expr -> 'a)
    -> TE.t
    -> E.t
    -> P.binding list
    -> P.expr
    -> 'a
end = struct
  let let_binding expr tenv env binds e =
    let module B = Bindings in
    let half_typed_binds, binds_env = B.explicit_annotations tenv binds in
    let typed_binds =
      List.map
        (fun (x, constr, rhs) ->
           match constr with
           | None ->
             (x, constr, Infer.expr tenv (E.merge env binds_env) rhs)
           | Some ty ->
             Check.expr tenv (E.merge env binds_env) rhs ty;
             (x, constr, ty)
        )
        half_typed_binds
    in
    let added_env = B.report_inference_results typed_binds in
    expr tenv (E.merge env added_env) e
end

module P = Simple.Ast
module E = Typing_env
module L = Parse.Location
module T = Types
module TE = T.Environment
module W = L.With_loc

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

let check_subtype loc ~inferred ~expected =
  if not (Types.sub inferred expected) then
    typeError loc
      "This expression has type %s while a subtype of %s was expected"
      (Types.show inferred)
      (Types.show expected)

module Bindings = struct
  let explicit_annotations (tenv : TE.t) (bindings : P.binding list)
    : ((string * Types.t option * P.expr) list * E.t, string) result =
    let half_typed_bindings =
      List.map
        (fun ((var, maybe_annot), e) ->
           let ret_triple x = (var, Some x, e) in
           CCOpt.map (Annotations.to_type tenv) maybe_annot
           |> CCOpt.map_or
             ~default:(Ok (var, None, e))
             (CCResult.map2 ret_triple CCFun.id)
        )
        bindings
      |> List.fold_left (fun accu elt ->
          CCResult.both elt accu |> CCResult.map @@ CCFun.uncurry CCList.cons)
        (Ok [])
    in
    CCResult.map
      (fun half_typed_bindings ->
         let new_env =
           List.fold_left
             (fun accu (x, constr, _) ->
                E.add x (CCOpt.get_or ~default:Types.Builtins.grad constr) accu)
             E.empty
             half_typed_bindings
         in
         half_typed_bindings, new_env
      )
      half_typed_bindings

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
  | P.Cstring s ->
    (* Cduce strings are lists of chars, we can keep it the same *)
    CCString.fold
      (fun accu char -> T.Builtins.cons
          (T.node @@ Cduce_lib.Types.char
             (Cduce_lib.Chars.(atom @@ V.mk_char char)))
          (T.node accu))
      T.Builtins.nil
      (CCString.rev s)

let () = ()
(* For some reason, ocp-indent don't work correctly if we don't put this here *)

(* [get_discriminer t] returns [Some t1] if [t] is of the form
   `(t1 -> true) & (not t1 -> false)`, and [None] otherwise. *)
and get_discriminer typ =
  if T.sub typ T.Builtins.(arrow (T.node empty) (T.node any)) then
    let (_, arrows) = Cduce_lib.Types.Arrow.get typ in
    match arrows with
    | [[ (t1, b1); (t2, b2) ]]
      when T.equiv b1 T.Builtins.true_type &&
           T.equiv b2 T.Builtins.false_type
           && T.equiv t2 (T.Builtins.neg t1)
      -> Some t1
    | [[ (t2, b2); (t1, b1) ]]
      when T.equiv b1 T.Builtins.true_type &&
           T.equiv b2 T.Builtins.false_type
           && T.equiv t2 (T.Builtins.neg t1)
      -> Some t1
    | _ -> None
  else None

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
      let (added_env, domain) =
        CCResult.catch (Pattern.infer tenv pat)
          ~ok:CCFun.id
          ~err:(fun err -> typeError e.L.With_loc.location "%s" err)
      in
      let codomain = expr tenv (E.merge env added_env) e in
      Types.(Builtins.arrow (node domain) (node codomain))
    | P.EfunApp (e1, e2) ->
      let t1 = expr tenv env e1
      and t2 = expr tenv env e2
      in
      if not @@ Types.sub t1 Types.(Builtins.(arrow (node empty) (node any)))
      then
        typeError e.L.With_loc.location
          "This expression has type %s which is not an arrow type. \
           It can't be applied"
          (Types.show t1);
      let t1arrow = Cduce_lib.Types.Arrow.get t1 in
      let dom = Cduce_lib.Types.Arrow.domain t1arrow in
      check_subtype e2.L.With_loc.location ~inferred:t2 ~expected:dom;
      Cduce_lib.Types.Arrow.apply t1arrow t2
    | P.Elet (binds, e) ->
      Common.let_binding expr tenv env binds e
    | P.EopApp (op, args) ->
      operator tenv env e.L.With_loc.location op args
    | P.Eite (e0, e1, e2) ->
      if_then_else tenv env e0 e1 e2
    | P.EtyAnnot (sub_e, annot) ->
      let t =
        CCResult.catch
          ~err:(fun err -> typeError e.L.With_loc.location "%s" err)
          ~ok:CCFun.id
          (Annotations.to_type tenv annot)
      in
      check_subtype
        sub_e.L.With_loc.location
        ~expected:t
        ~inferred:(expr tenv env sub_e);
      t

    | P.EaccessPath _
    | P.Erecord _
    | P.Ewith (_,_) -> assert false

  and operator tenv env loc op args = match op, args with
    | P.Ocons, [e1; e2] ->
      let t1 = expr tenv env e1
      and t2 = expr tenv env e2
      in
      check_subtype
        loc
        ~inferred:t2
        ~expected:Types.(Builtins.(cup (cons (node any) (node any)) nil));
      Types.Builtins.(cons (Types.node t1) (Types.node t2))
    | P.Oeq, [e1; e2] ->
      let t1 = expr tenv env e1
      and t2 = expr tenv env e2
      in
      ignore t1; ignore t2;
      Types.Builtins.bool
    | P.Oneg, [e] ->
      let t = expr tenv env e in
      check_subtype
        loc
        ~inferred:t
        ~expected:Types.Builtins.int;
      let ivl = Cduce_lib.Types.Int.get t in
      let negated_ivl =
        Cduce_lib.Types.VarIntervals.compute
          ~empty:Cduce_lib.Intervals.empty
          ~full:Cduce_lib.Intervals.full
          ~cup:Cduce_lib.Intervals.cup
          ~cap:Cduce_lib.Intervals.cap
          ~diff:Cduce_lib.Intervals.diff
          ~atom:(function
              | `Atm i -> Cduce_lib.Intervals.negat i
              | `Var _ -> assert false (* XXX: What are those vars ? *))
          ivl
      in Cduce_lib.Types.interval negated_ivl
    | P.Oplus, [e1; e2]
    | P.Ominus, [e1; e2]
      ->
      let t1 = expr tenv env e1
      and t2 = expr tenv env e2
      in
      check_subtype loc ~inferred:t1 ~expected:T.Builtins.int;
      check_subtype loc ~inferred:t2 ~expected:T.Builtins.int;
      T.Builtins.int
    | P.Oplus, _
    | P.Ominus, _
    | P.Ocons, _
    | P.Oeq, _
    | P.Oneg, _
      -> assert false (* This isn't supposed to happend *)

  and if_then_else tenv env e0 e1 e2 =
    (* [type_with_exfalso var typ e] types [e] using current env + the
     * hypothesis [var:typ], and an exfalso rule stating that if [typ] is
     * [empty], then [e] can be given any type -- and in particular [empty] *)
    let type_with_exfalso var typ e =
      if Types.equiv typ Types.Builtins.empty then
        Types.Builtins.empty
      else
        expr tenv (E.add var typ env) e
    in
    let type_default () =
      let t0 = expr tenv env e0 in
      let t1 = type_with_exfalso "_" Types.Builtins.(cap t0 true_type) e1
      and t2 = type_with_exfalso "_" Types.Builtins.(cap t0 false_type) e2
      in
      check_subtype
        e0.L.With_loc.location
        ~inferred:t0
        ~expected:Types.Builtins.bool;
      Types.Builtins.cup t1 t2
    in
    match W.description e0 with
    | P.EfunApp (f, ({ W.description = P.Evar x; _ } as e_x)) ->
      let t_f = expr tenv env f in
      begin
        match get_discriminer t_f with
        | Some t ->
          let t_x = expr tenv env e_x in
          let t1 = type_with_exfalso x Types.Builtins.(cap t_x t) e1
          and t2 = type_with_exfalso x Types.Builtins.(cap t_x (neg t)) e2
          in Types.Builtins.cup t1 t2
        | None -> type_default ()
      end
    | _ -> type_default ()
end

and Check : sig
  val expr : TE.t -> E.t -> P.expr -> Types.t -> unit
end = struct
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
          let (added_env, _) =
            CCResult.catch (Pattern.infer ~t_constr:dom tenv pat)
              ~ok:CCFun.id
              ~err:(fun err -> typeError e.L.With_loc.location "%s" err)
          in
          let _typed_e = expr tenv (E.merge env added_env) e codom in
          ())
        (a_op expected_arrow)
    | P.Elet (binds, e) ->
      Common.let_binding expr tenv env binds e expected
    | P.Eite (e0, e1, e2) ->
      if_then_else tenv env e0 e1 e2 expected
    | P.EfunApp (e1, e2) ->
      let t1 = Infer.expr tenv env e2 in
      expr tenv env e1 Types.(Builtins.arrow (node t1) (node expected))
    | P.EopApp (op, args) ->
      operator tenv env (L.With_loc.loc e) op args expected
    | P.EaccessPath _
    | P.Erecord _
    | P.Ewith (_,_)
    | P.EtyAnnot (_,_) -> assert false

  and operator tenv env loc op args expected =
    match op, args with
    | P.Ocons, [e1; e2] ->
      let products = Cduce_lib.Types.Product.get expected in
      if
        CCList.for_all
          (fun (t1, t2) ->
             try
               expr tenv env e1 t1;
               expr tenv env e2 t2;
               false
             with
               TypeError _ -> true
          )
          products
      then
        typeError loc "This expression should have type %s" @@ T.show expected
    | P.Oeq, [e1; e2] ->
      check_subtype
        loc
        ~inferred:expected
        ~expected:Types.Builtins.bool;
      if T.equiv expected Types.Builtins.true_type then
        typeError loc "Can't check thas this equality always holds"
      else if T.equiv expected Types.Builtins.false_type then
        typeError loc "Can't check thas this equality never holds";
      let t1 = Infer.expr tenv env e1
      and t2 = Infer.expr tenv env e2
      in
      ignore (t1, t2)
    | P.Oneg, [e] ->
      check_subtype
        loc
        ~inferred:expected
        ~expected:Types.Builtins.int;
      (* We just check that [e] has type [-expected] *)
      let ivl = Cduce_lib.Types.Int.get expected in
      let negated_ivl =
        Cduce_lib.Types.VarIntervals.compute
          ~empty:Cduce_lib.Intervals.empty
          ~full:Cduce_lib.Intervals.full
          ~cup:Cduce_lib.Intervals.cup
          ~cap:Cduce_lib.Intervals.cap
          ~diff:Cduce_lib.Intervals.diff
          ~atom:(function
              | `Atm i -> Cduce_lib.Intervals.negat i
              | `Var _ -> assert false (* XXX: What are those vars ? *))
          ivl
      in
      expr tenv env e (Cduce_lib.Types.interval negated_ivl)
    | P.Oplus, [e1; e2]
    | P.Ominus, [e1; e2] ->
      check_subtype
        loc
        ~inferred:expected
        ~expected:T.Builtins.int;
      ignore @@ List.map (fun e -> expr tenv env e T.Builtins.int) [e1; e2]
    | P.Oplus, _
    | P.Ominus, _
    | P.Oneg, _
    | P.Oeq, _ -> assert false
    | P.Ocons, _ -> assert false

  and if_then_else tenv env e0 e1 e2 expected =
    (* [check_with_exfalso var typ e expected] checks [e] against the type
     * [expected] using current env + the hypothesis [var:typ], and an exfalso
     * rule stating that if [typ] is [empty], then [e] can be given any type --
     * and in particular [empty] *)
    let check_with_exfalso var typ e expected =
      if not @@ Types.equiv typ Types.Builtins.empty then
        expr tenv (E.add var typ env) e expected;
    in
    let default () =
      let t0 = Infer.expr tenv env e0 in
      check_subtype
        (L.With_loc.loc e0)
        ~inferred:t0
        ~expected:T.Builtins.bool;
      check_with_exfalso "_" T.Builtins.(cap t0 true_type) e1 expected;
      check_with_exfalso "_" T.Builtins.(cap t0 false_type) e2 expected
    in
    match L.With_loc.description e0 with
    | P.EfunApp (f, ({ W.description = P.Evar x; _ } as e_x)) ->
      let t_f = Infer.expr tenv env f in
      begin
        match get_discriminer t_f with
        | Some t ->
          let t_x = Infer.expr tenv env e_x in
          check_with_exfalso x Types.Builtins.(cap t_x t) e1 expected;
          check_with_exfalso x Types.Builtins.(cap t_x (neg t)) e2 expected
        | None -> default ()
      end
    | _ -> default ()
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
    let half_typed_binds, binds_env =
      CCResult.catch (B.explicit_annotations tenv binds)
        ~ok:CCFun.id
        ~err:(fun err -> typeError e.L.With_loc.location "%s" err)
    in
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

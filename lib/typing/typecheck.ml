module C = Common
module P = Simple.Ast
module E = Environment
module L = Common.Location
module Loi = List_or_infinite
module T = Types
module TE = T.Environment
module VE = Typing_env
module WL = L.With_loc
module Warning = Common.Warning

module Pattern = Typecheck_pat

module W = Common.Writer.Make(Common.Warning.List)

open W.Infix
let (>>) e1 e2 = e1 >>= (fun _ -> e2)

module Flagged =
struct
  (** The boolean flag is false by default, and may be marked true *)
  type 'a t = 'a * bool

  let pure x = (x, false)

  let flagged x = (x, true)
end
module FlaggedWriter =
struct
  type 'a t = 'a Flagged.t W.t

  let pure x = W.pure @@ Flagged.pure x
  let bind (f : 'a -> 'b t) : 'a t -> 'b t =
    W.bind (fun (x, flag) ->
        W.map
          (fun (x', flag') -> (x', flag || flag'))
          (f x))
  let map f = bind (fun x -> pure @@ f x)

  (* let lift x = W.map Flagged.pure x *)
  let lift_flagged x = W.map Flagged.flagged x

  let map_l f l =
    let rec lift_list_option = function
      | [] -> ([], false)
      | hd::tl ->
        let (hd_elt, hd_flg) = hd
        and (tl_elt, tl_flg) = lift_list_option tl in
        (hd_elt::tl_elt, hd_flg || tl_flg)
    in
    W.map_l f l >|=
    lift_list_option

  module Infix =
  struct
    let (>>=) x f = bind f x
    let (>|=) x f = map  f x
    let (>>) e1 e2 = e1 >>= fun _ -> e2
  end
end

let default_typ = T.Builtins.grad
let log_only l =
  W.append l @@ W.pure default_typ

let check_subtype loc ~inferred ~expected =
  if Types.sub inferred expected then
    W.pure ()
  else
    W.append
      [Warning.format loc
         "This expression has type %s while a subtype of %s was expected"
         (Types.show inferred)
         (Types.show expected)]
      (W.pure ())

module Bindings = struct
  let explicit_annotations (tenv : TE.t) (bindings : P.binding list)
    : ((string * Types.t option * P.expr) list * VE.t) W.t =
    let half_typed_bindings =
      List.map
        (fun ((var, maybe_annot), e) ->
           CCOpt.map (Annotations.to_type tenv) maybe_annot
           |> CCOpt.map_or
             ~default:(W.pure (var, None, e))
             (W.map (fun x -> (var, Some x, e)))
        )
        bindings
      |> List.fold_left (CCFun.flip @@ W.map2 CCList.cons) (W.pure [])
    in
    W.map
      (fun half_typed_bindings ->
         let new_env =
           List.fold_left
             (fun accu (x, annot, _) ->
                VE.add x (CCOpt.get_or ~default:Types.Builtins.grad annot) accu)
             VE.empty
             half_typed_bindings
         in
         half_typed_bindings, new_env
      )
      half_typed_bindings

  let report_inference_results
      (typed_binds : (string * Types.t option * Types.t) list)
    : VE.t =
    List.fold_left
      (fun accu (x, constr, rhs_typ) ->
         VE.add x (CCOpt.get_or ~default:rhs_typ constr) accu)
      VE.empty
      typed_binds
end

let typeof_const = function
  | P.Cbool b -> T.Singleton.bool b
  | P.Cint  i -> T.Singleton.int i
  | P.Cstring s -> Types.Singleton.string s
  | P.Cpath s -> Types.Singleton.path s
  | P.Cbracketed _ -> Types.Path.any
  | P.Cundef -> Types.Builtins.undef

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
  val expr : Environment.t -> P.expr -> Types.t W.t

  val typeError : L.t -> ('a, unit, string, Types.t W.t) format4 -> 'a
end = struct

  let typeError loc e = Format.ksprintf
      (fun s -> log_only [Warning.make loc s])
      e

  let rec expr (env : Environment.t) (e : P.expr) : Types.t W.t =
    let { Environment.types = tenv; values = venv; _ } = env in
    let loc = e.L.With_loc.location in
    L.With_loc.description e |> function
    | P.Econstant c -> W.pure @@ typeof_const c
    | P.Evar v ->
      CCOpt.map_or
        ~default:(typeError loc "Unbound variable %s" v)
        W.pure
        (VE.lookup venv v)
    | P.Elambda (pat, e) ->
      (Pattern.infer tenv pat) >>= fun (added_env, domain) ->
      expr { env with E.values = VE.merge venv added_env } e >|= fun codomain ->
      Types.(Builtins.arrow (node domain) (node codomain))
    | P.EfunApp (e1, e2) ->
      expr env e1 >>= fun t1 ->
      expr env e2 >>= fun t2 ->
      if not @@ Types.sub t1 Types.(Builtins.(arrow (node empty) (node any)))
      then
        typeError loc
          "This expression has type %s which is not an arrow type. \
           It can't be applied"
          (Types.show t1)
      else
        let t1arrow = T.get_arrow t1 in
        let dom = Cduce_lib.Types.Arrow.domain t1arrow in
        check_subtype e2.L.With_loc.location ~inferred:t2 ~expected:dom >>
        W.pure @@ T.arrow_apply t1arrow t2
    | P.Elet (binds, e) ->
      Common.let_binding expr env binds e
      |> W.join
    | P.Ebinop (op, e1, e2) ->
      binop env loc op e1 e2
    | P.Emonop (op, e) ->
      monop env loc  op e
    | P.Eite (e0, e1, e2) ->
      if_then_else env e0 e1 e2
    | P.EtyAnnot (sub_e, annot) ->
      Annotations.to_type tenv annot >>= fun t ->
      expr env sub_e >>= fun inferred ->
      check_subtype sub_e.L.With_loc.location ~expected:t ~inferred >>
      W.pure @@ t
    | P.Epragma (pragma, e) ->
      let env = Common.pragma env pragma in
      expr env e
    | P.Eimport e -> Common.import expr default_typ env e
    | P.Erecord fields ->
      record env fields
    | P.EaccessPath (e, ap, default) ->
      W.map_opt (expr env) default >>= fun default_ty ->
      let unguarded_typing =
        Common.record_access env e ap (CCOpt.is_some default_ty)
      in
      let final_type =
        begin match (default_ty, W.value unguarded_typing) with
          | None, (_, true) ->
            typeError loc "Couldn't select this field"
          | _, (t, false) ->
            W.return t
          | Some tdef, (t, true) ->
            W.return @@ T.Builtins.cup t tdef
        end
      in W.append
        (W.log unguarded_typing)
        final_type
    | P.Ewith _ ->
      typeError loc "With constructs are not allowed"

  and binop env loc op e1 e2 = match op with
    | P.Ocons ->
      expr env e1 >>= fun t1 ->
      expr env e2 >>= fun t2 ->
      check_subtype
        loc
        ~inferred:t2
        ~expected:Types.(Builtins.(cup (cons (node any) (node any)) nil)) >>
      W.pure Types.Builtins.(cons (Types.node t1) (Types.node t2))
    | P.Oeq ->
      expr env e1 >>
      expr env e2 >>
      W.pure Types.Builtins.bool
    | P.Oplus
    | P.Ominus
      ->
      expr env e1 >>= fun t1 ->
      expr env e2 >>= fun t2 ->
      check_subtype loc ~inferred:t1 ~expected:T.Builtins.int >>
      check_subtype loc ~inferred:t2 ~expected:T.Builtins.int >>
      W.pure @@ T.Builtins.int
    | P.Oor
    | P.Oand ->
      expr env e1 >>= fun t1 ->
      expr env e2 >>= fun t2 ->
      check_subtype loc ~inferred:t1 ~expected:T.Builtins.bool >>
      check_subtype loc ~inferred:t2 ~expected:T.Builtins.bool >>
      let op = if op = P.Oand then T.Bool.tand else T.Bool.tor in
      begin try
          W.pure @@ op t1 t2
        with Invalid_argument _ -> W.pure T.Bool.all
      end
    | P.OrecordMember ->
      let record = e1 and fname_expr = e2 in
      expr env fname_expr >>= fun t1 ->
      expr env record >>= fun t_record ->
      check_subtype loc ~inferred:t1 ~expected:T.Builtins.string >>
      check_subtype loc ~inferred:t_record ~expected:T.Record.any >>
      begin match T.String.get t1 with
        | Infinite -> W.pure @@ T.Bool.all
        | Finite strings ->
          let memberships = CCList.map
              (fun s ->
                 T.sub t_record
                   (T.Record.of_list true [ (false, s, T.Builtins.any) ]))
              strings
          in
          if CCList.for_all ((=) true) memberships
          then
            W.pure @@ T.Bool.true_type
          else if CCList.for_all ((=) false) memberships then
            W.pure @@ T.Bool.false_type
          else W.pure @@ T.Bool.all
      end
    | P.Omerge ->
      expr env e1 >>= fun t1 ->
      expr env e2 >>= fun t2 ->
      check_subtype loc ~inferred:t1 ~expected:T.Record.any >>
      check_subtype loc ~inferred:t2 ~expected:T.Record.any >>
      W.pure @@ T.Record.merge t1 t2

  and monop env loc op e = match op with
    | P.Oneg ->
      expr env e >>= fun t ->
      check_subtype
        loc
        ~inferred:t
        ~expected:Types.Builtins.int >>
      let ivl = Cduce_lib.Types.Int.get t in
      let negated_ivl = Common.negate_interval ivl in
      W.pure @@ negated_ivl
    | P.Onot ->
      expr env e >>= fun t ->
      check_subtype loc ~inferred:t ~expected:T.Builtins.bool >>
      begin try
          W.pure @@ T.Bool.tnot t
        with Invalid_argument _ -> W.pure T.Bool.all
      end

  and if_then_else env e0 e1 e2 =
    (* [type_with_exfalso var typ e] types [e] using current env + the
     * hypothesis [var:typ], and an exfalso rule stating that if [typ] is
     * [empty], then [e] can be given any type -- and in particular [empty] *)
    let type_with_exfalso var typ e =
      if Types.equiv typ Types.Builtins.empty then
        W.pure Types.Builtins.empty
      else
        expr (E.add_value env var typ) e
    in
    let type_default () =
      expr env e0 >>= fun t0 ->
      type_with_exfalso "_" Types.Builtins.(cap t0 true_type) e1 >>= fun t1 ->
      type_with_exfalso "_" Types.Builtins.(cap t0 false_type) e2 >>= fun t2 ->
      check_subtype
        e0.L.With_loc.location
        ~inferred:t0
        ~expected:Types.Builtins.bool >>
      W.pure @@ Types.Builtins.cup t1 t2
    in
    match WL.description e0 with
    | P.EfunApp (f, ({ WL.description = P.Evar x; _ } as e_x)) ->
      expr env f >>= fun t_f ->
      begin
        match get_discriminer t_f with
        | Some t ->
          expr env e_x >>= fun t_x ->
          type_with_exfalso x Types.Builtins.(cap t_x t) e1 >>= fun t1 ->
          type_with_exfalso x Types.Builtins.(cap t_x (neg t)) e2 >|= fun t2 ->
          Types.Builtins.cup t1 t2
        | None -> type_default ()
      end
    | _ -> type_default ()

  and record env fields =
    let typed_fields = W.map_l (field env) fields in
    let typed_labels = W.map (fun l -> fst @@ CCList.split l) typed_fields in
    let typed_fields = snd @@ CCList.split (W.value typed_fields) in
    check_empty_intersection typed_labels >>
    let label_choices =
      W.map (List.map (fun t -> T.String.get (WL.description t)))
        typed_labels
    in
    let possible_combinations =
      W.map (CCList.fold_left
               (fun accu labels_n ->
                  Loi.flat_map (fun label ->
                      Loi.map
                        (fun partial_sequence -> label :: partial_sequence)
                        accu)
                    labels_n)
               (Loi.finite [[]]))
        label_choices
      |> W.map (Loi.map List.rev)
    in
    W.map (Loi.fold (fun partial_typ combination ->
        let new_typ =
          T.Builtins.record false @@
          Simple.Record.of_list @@
          CCList.map2 (fun label_str field_type ->
              (label_str, T.node @@ WL.description field_type))
            combination
            typed_fields
        in
        T.Builtins.cup
          partial_typ
          new_typ)
        ~init:T.Builtins.empty
        ~full:T.Builtins.any)
      possible_combinations

  and field
      (env : Environment.t)
      ((label, maybe_annot, e) : (P.expr * 'a * P.expr))
    : (T.t WL.t * T.t WL.t) W.t =
    let infer_keeping_loc env e = W.map (WL.mk (WL.loc e)) (expr env e)
    and check_keeping_loc env expected e =
      W.map (WL.mk (WL.loc e)) (Check.expr env e expected) >|=
      WL.map (CCFun.const expected)
    in
    match maybe_annot with
    | None ->
      W.map_pair (infer_keeping_loc env) (infer_keeping_loc env) (label, e)
    | Some annot ->
      Annotations.to_type env.Environment.types annot >>= fun expected ->
      W.map_pair
        (infer_keeping_loc env)
        (check_keeping_loc env expected)
        (label, e)

  and check_empty_intersection :
    Types.t WL.t list W.t -> unit W.t
    =
    let distinct_from typ =
      W.iter_l @@ fun typ' ->
      if Cduce_lib.Types.disjoint
          (WL.description typ)
          (WL.description typ')
      then
        W.pure ()
      else
        W.pure () |>
        W.append [
          Format.kasprintf
            (Warning.make ~kind:Warning.Error (WL.loc typ))
            "This label and the one at %a may be the same"
            L.pp (WL.loc typ')
        ]
    in
    let rec aux =
      function
      | [] -> W.pure ()
      | located_typ::tl ->
        distinct_from located_typ tl >>
        aux tl
    in W.bind aux
end

and Check : sig
  val expr : E.t -> P.expr -> Types.t -> unit W.t
end = struct

  let typeError loc e = Format.kasprintf
      (fun s -> W.append [Warning.make loc s] (W.pure ())) e


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

  let rec expr env e expected =
    let loc = L.With_loc.loc e in
    L.With_loc.description e |> function
    | P.Econstant c ->
      let c_ty = typeof_const c in
      check_subtype loc ~inferred:c_ty ~expected
    | P.Evar v ->
      begin match VE.lookup env.E.values v with
        | Some t -> check_subtype loc ~inferred:t ~expected
        | None ->
          W.append
            [Warning.format e.L.With_loc.location "Unbound variable %s" v]
            (W.pure ())
      end
    | P.Elambda (pat, e) ->
      (check_subtype loc ~inferred:expected ~expected:Cduce_lib.Types.Arrow.any
       >>
       let expected_arrow = Cduce_lib.Types.Arrow.get expected in
       W.iter_l (fun (dom, codom) ->
           Pattern.infer ~t_constr:dom env.E.types pat
           >>= fun (added_env, _) ->
           expr (E.add_values env added_env) e codom)
         (a_op expected_arrow))
      >> W.pure ()
    | P.Elet (binds, e) ->
      Common.let_binding expr env binds e >>= fun f -> f expected
    | P.Eite (e0, e1, e2) ->
      if_then_else env e0 e1 e2 expected
    | P.EfunApp (e1, e2) ->
      Infer.expr env e2 >>= fun t1 ->
      expr env e1 Types.(Builtins.arrow (node t1) (node expected))
    | P.Ebinop (op, e1, e2) ->
      binop env (L.With_loc.loc e) op e1 e2 expected
    | P.Emonop (op, e) ->
      monop env (L.With_loc.loc e) op e expected
    | P.Epragma (pragma, e) ->
      let env = Common.pragma env pragma in
      expr env e expected
    | P.Eimport e ->
      Common.import (fun env e -> expr env e expected) () env e
    | P.EtyAnnot (e, annot) ->
      Annotations.to_type env.Environment.types annot >>= fun ty ->
      check_subtype loc ~inferred:ty ~expected >>
      expr env e ty
    | P.Erecord fields ->
      record env fields loc expected
    | P.EaccessPath (e, ap, default) ->
      let unguarded_typing =
        Common.record_access env e ap (CCOpt.is_some default)
      in
      let final_check =
        begin match (default, W.value unguarded_typing) with
          | None, (_, true) ->
            typeError loc "Couldn't select this field"
          | _, (t, false) ->
            check_subtype loc ~inferred:t ~expected
          | Some guard, (t, true) ->
            check_subtype loc ~inferred:t ~expected >>
            expr env guard expected
        end
      in W.append
        (W.log unguarded_typing)
        final_check
    | P.Ewith (_,_) ->
      typeError loc "With constructs are not allowed"

  and binop env loc op e1 e2 expected =
    match op with
    | P.Ocons ->
      let products = Cduce_lib.Types.Product.get expected in
      if
        CCList.for_all
          (fun (t1, t2) ->
             expr env e1 t1 >>
             expr env e2 t2
             |> W.log |> Warning.List.contains_error
          )
          products
      then
        typeError loc "This expression should have type %s" @@ T.show expected
      else W.pure ()
    | P.Oeq ->
      check_subtype
        loc
        ~inferred:expected
        ~expected:Types.Builtins.bool >>
      if T.equiv expected Types.Builtins.true_type then
        typeError loc "Can't check thas this equality always holds"
      else if T.equiv expected Types.Builtins.false_type then
        typeError loc "Can't check thas this equality never holds"
      else
        Infer.expr env e1 >>
        Infer.expr env e2 >> W.pure ()
    | P.Oplus
    | P.Ominus ->
      check_subtype
        loc
        ~inferred:expected
        ~expected:T.Builtins.int >>
      W.pure @@
      ignore @@ List.map (fun e -> expr env e T.Builtins.int) [e1; e2]
    | P.Oor
    | P.Oand ->
      (* The [and] and [or] operators work the same way, but with the roles of
         [true] and [false] inverted *)
      let (top, bottom) = if op = P.Oand
        then T.Builtins.(true_type, false_type)
        else T.Builtins.(false_type, true_type)
      in
      let bool_part = T.Builtins.cap expected T.Builtins.bool in
      if T.sub bool_part top then
        expr env e1 top >>
        expr env e2 top
      else if T.sub bool_part bottom then
        (* We first try to check that the first operant has type [bottom].
           If it fails, we try to check that the second has type [bottom] and
           the first has type [Bool] *)
        let first_try = expr env e1 bottom in
        let first_log = W.log first_try in
        if CCList.exists (fun w -> Warning.(get_kind w = Error)) first_log then
          expr env e1 T.Bool.all >>
          expr env e2 bottom
        else
          expr env e2 T.Bool.all
      else
        expr env e1 T.Bool.all >>
        expr env e2 T.Bool.all
    | P.OrecordMember ->
      let record = e1 and fname_expr = e2 in
      Infer.expr env fname_expr >>= fun t1 ->
      check_subtype loc ~inferred:t1 ~expected:T.Builtins.string >>
      let bool_part = T.Builtins.cap expected T.Builtins.bool in
      let is_true = T.sub bool_part T.Bool.true_type
      and is_false = T.sub bool_part T.Bool.false_type
      in
      if is_true || is_false then
        begin match T.String.get t1 with
          | Infinite ->
            typeError loc
              (if is_true then
                 "Can't prove that this field is a member of this record"
               else
                 "Can't prove that this field isn't a member of this record"
              )
          | Finite strings ->
            let atomic_record f_name =
              let singleton_record =
                T.Record.of_list true [ (false, f_name, T.Builtins.any) ]
              in
              if is_true
              then singleton_record
              else T.Builtins.neg singleton_record
            in
            (W.map_l (fun s ->
                 expr env record (atomic_record s))
                strings) >> W.return ()
        end
      else expr env record T.Record.any
    | P.Omerge ->
      Infer.expr env e1 >>= fun t1 ->
      Infer.expr env e2 >>= fun t2 ->
      check_subtype loc ~inferred:t1 ~expected:T.Record.any >>
      check_subtype loc ~inferred:t2 ~expected:T.Record.any >>
      let result = T.Record.merge t1 t2 in
      check_subtype loc ~inferred:result ~expected

  and monop env loc op e expected = match op with
    | P.Onot ->
      let bool_part = T.Builtins.cap expected T.Builtins.bool in
      expr env e (T.Bool.tnot bool_part)
    | P.Oneg ->
      (* FIXME: Isn't this check absurd? *)
      check_subtype
        loc
        ~inferred:expected
        ~expected:Types.Builtins.int >>
      (* We just check that [e] has type [-expected] *)
      let ivl = Cduce_lib.Types.Int.get expected in
      let negated_ivl = Common.negate_interval ivl in
      expr env e negated_ivl

  and if_then_else env e0 e1 e2 expected =
    (* [check_with_exfalso var typ e expected] checks [e] against the type
     * [expected] using current env + the hypothesis [var:typ], and an exfalso
     * rule stating that if [typ] is [empty], then [e] can be given any type --
     * and in particular [empty] *)
    let check_with_exfalso var typ e expected =
      if Types.equiv typ Types.Builtins.empty then
        W.pure ()
      else
        expr (E.add_value env var typ) e expected
    in
    let default () =
      Infer.expr env e0 >>= fun t0 ->
      check_subtype
        (L.With_loc.loc e0)
        ~inferred:t0
        ~expected:T.Builtins.bool >>
      check_with_exfalso "_" T.Builtins.(cap t0 true_type) e1 expected >>
      check_with_exfalso "_" T.Builtins.(cap t0 false_type) e2 expected
    in
    match L.With_loc.description e0 with
    | P.EfunApp (f, ({ WL.description = P.Evar x; _ } as e_x)) ->
      Infer.expr env f >>= fun t_f ->
      begin
        match get_discriminer t_f with
        | Some t ->
          Infer.expr env e_x >>= fun t_x ->
          check_with_exfalso x Types.Builtins.(cap t_x t) e1 expected >>
          check_with_exfalso x Types.Builtins.(cap t_x (neg t)) e2 expected
        | None -> default ()
      end
    | _ -> default ()

  and record env fields loc expected =
    let to_assoc = List.map
        (fun (x1, x2, x3) -> (x1, (x2, x3)))
    in
    let assoc_fields = to_assoc fields in
    let (labels, annotated_values) = List.split assoc_fields in
    let annotated_values = Array.of_list annotated_values in
    let expr_keeping_loc env e  =
      Infer.expr env e  >|= fun t -> (t, WL.loc e)
    in
    W.map_l (fun e -> (expr_keeping_loc env e)) labels >>= fun labels_ty ->
    let to_singleton (x, loc) = match T.String.get x with
      | Finite [s] -> W.return (s, loc)
      | Finite _ | Infinite ->
        typeError loc "Can only check records with static labels" (* TODO *) >>
        W.return ("", loc)
    in
    W.map_l to_singleton labels_ty >>= fun labels ->
    let labelMap_wl = CCList.fold_left
        (fun map_wl (lbl, loc) ->
           let map = W.value map_wl in
           match C.StrMap.add_or lbl loc map with
           | Ok map -> W.return map
           | Error (_, _, loc2) ->
             typeError loc
               "This label and the one defined at %a are the same" L.pp loc2 >>
             W.return map)
        (W.return C.StrMap.empty)
        labels
    in
    labelMap_wl >>= fun labelMap ->
    W.iter_l (fun lbl ->
        if C.StrMap.mem lbl labelMap then
          W.return ()
        else
          typeError loc "Missing field %s" lbl)
      (T.Record.labels expected) >>
    let result_list = CCList.mapi (fun idx (label, loc) ->
        let (annot, e) = annotated_values.(idx) in
        let expected_field_type = T.Record.get_field label expected in
        let real_expr = match annot with
          | None   -> e
          | Some a -> WL.mk loc (P.EtyAnnot (e, a))
        in
        expr env real_expr expected_field_type
      )
        labels
    in W.map_l (CCFun.id) result_list >>
    W.return ()
end

and Common : sig
  val let_binding : (E.t -> P.expr -> 'a)
    -> E.t
    -> P.binding list
    -> P.expr
    -> 'a W.t
  val pragma : E.t -> Parse.Pragma.t -> E.t
  val import : (E.t -> P.expr -> 'a W.t) -> 'a -> E.t -> P.expr -> 'a W.t
  val negate_interval : Cduce_lib.Types.VarIntervals.t -> T.t
  val record_access : E.t -> P.expr -> P.access_path -> bool -> T.t FlaggedWriter.t
end = struct

  let let_binding expr env binds e =
    let module B = Bindings in
    B.explicit_annotations env.E.types binds >>=
    fun (half_typed_binds, binds_env) ->
    let new_env = { env with E.values = VE.merge env.E.values binds_env } in
    let typed_binds =
      List.map
        (fun (x, constr, rhs) ->
           match constr with
           | None ->
             Infer.expr new_env rhs >|= fun typed_rhs ->
             (x, constr, typed_rhs)
           | Some ty ->
             Check.expr new_env rhs ty >>
             W.pure (x, constr, ty)
        )
        half_typed_binds
      |> List.fold_left
        (CCFun.flip @@ W.map2 CCList.cons)
        (W.pure [])
    in
    typed_binds >|= fun binds ->
    let added_env = B.report_inference_results binds in
    expr (E.add_values env added_env) e

  let pragma env =
    let module P = Parse.Pragma in
    function
    | P.Warnings warns ->
      E.map_config (fun c -> Config.proceed_warnings_annot c warns) env
    | P.Errors warns ->
      E.map_config (fun c -> Config.proceed_errors_annot c warns) env

  let import expr default_value env e =
    let typeError loc e = Format.ksprintf
        (fun s -> W.append [Warning.make loc s] (W.pure default_value)) e
    in
    let loc = WL.loc e in
    let current_path = Filename.dirname loc.L.file_name in
    Infer.expr env e >>= fun t ->
    check_subtype loc ~inferred:t ~expected:T.Builtins.(cup string path) >>
    let paths =
      List_or_infinite.concat
        (T.String.get t)
        (T.Path.get t)
    in
    match paths with
    | List_or_infinite.Finite [f_name] ->
      let absolute_f_name =
        if Filename.is_relative f_name then
          Filename.concat current_path f_name
        else f_name
      in
      begin try
          CCIO.with_in absolute_f_name (fun chan ->
              match
                MParser.parse_channel Parse.Parser.expr chan absolute_f_name
              with
              | MParser.Success e ->
                Simple.Of_onix.expr e >>= fun e ->
                expr Environment.default e
              | MParser.Failed (_, _) ->
                typeError e.WL.location "Parse error in %s"absolute_f_name )
        with Sys_error _ ->
          typeError e.WL.location "Unable to read file %s"absolute_f_name 
      end
    | _ -> typeError e.WL.location "Not a singleton string or path"

  let negate_interval =
    let module B = T.Builtins in
    Cduce_lib.Types.VarIntervals.compute
      ~empty:B.empty
      ~full:Cduce_lib.Types.Int.any
      ~cup:B.cup
      ~cap:B.cap
      ~diff:(fun t1 t2 -> B.cap t1 (B.neg t2))
      ~atom:(function
          | `Atm i ->
            Cduce_lib.Types.interval @@ Cduce_lib.Intervals.negat i
          | `Var v -> Cduce_lib.Types.var v)

  let record_access env e ap is_guarded : T.t FlaggedWriter.t =
    Infer.expr env e >>= fun te ->
    W.map_l (Infer.expr env) ap >>= fun tap ->
    let acces_path_singletons = List.map T.String.get tap in
    let acces_path_locations = List.map WL.loc ap in
    let access_path_wl =
      CCList.combine acces_path_singletons acces_path_locations
    in
    let module M = FlaggedWriter in
    let open M.Infix in
    let typeErrorIfUnguarded loc fmt_str =
      Format.ksprintf
        (fun s ->
           if is_guarded then
             M.lift_flagged (W.pure default_typ)
           else
             M.lift_flagged @@ Infer.typeError loc "%s" s)
        fmt_str
    in
    let rec aux record_type = function
      | [] -> M.pure record_type
      | apf::ap ->
        let loc = snd apf in
        let has_to_be_record =
          if T.sub record_type T.Record.any then
            M.pure ()
          else
            typeErrorIfUnguarded loc "This should be a record" >>
            M.pure ()
        in
        has_to_be_record >>
        begin match fst apf with
          | List_or_infinite.Finite strings ->
            let possible_accessed =
              List.map (fun s -> T.Record.get_field s record_type) strings
            in
            let process_type t =
              if is_guarded then begin
                if Cduce_lib.Types.Record.has_absent t then
                  M.lift_flagged (W.return @@ T.Builtins.diff t T.Record.absent)
                else
                  M.pure @@ T.Builtins.diff t T.Record.absent
              end
              else
              if Cduce_lib.Types.Record.has_absent t then
                typeErrorIfUnguarded loc "This field may be empty"
              else
                M.pure t
            in
            let sub_types = M.map_l process_type possible_accessed in
            sub_types >>=
            M.map_l (fun t -> aux t ap) >|= fun types ->
            CCList.fold_left T.Builtins.cup T.Builtins.empty types
          | Infinite ->
            if is_guarded then
              M.lift_flagged
              @@ W.pure
              @@ T.Builtins.diff (T.Record.all_values record_type) T.Record.absent
            else
              typeErrorIfUnguarded loc "Cannot determine the value of this field"
        end
    in aux te access_path_wl
end

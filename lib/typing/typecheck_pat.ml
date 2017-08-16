module P = Simple.Ast
module L = Common.Location.With_loc
module TE = Typing_env

module W = Common.Writer.Make(Common.Warning.List)

open W.Infix

let infer_var ?t_constr tenv maybe_t =
  match t_constr, maybe_t with
  | None, None -> W.pure Types.Builtins.grad
  | _, _ ->
    let real_constraint = CCOpt.get_or ~default:Types.Builtins.any t_constr
    and annoted =
      CCOpt.fold
        (fun _ annot -> Annotations.to_type tenv annot)
        (W.pure Types.Builtins.any)
        maybe_t
    in
    W.map (Types.Builtins.cap real_constraint) annoted

let infer_field ?t_constr tenv (name, (is_optional, maybe_annot)) =
  infer_var ?t_constr tenv maybe_annot >|= fun t ->
  let accepted_typ, produced_typ =
    if is_optional then
      let accepted_typ = Cduce_lib.Types.Record.or_absent t
      and produced_typ = Types.Builtins.cup t Types.Builtins.undef
      in (accepted_typ, produced_typ)
    else (t, t)
  in
  (TE.singleton name produced_typ, (name, Types.node accepted_typ))

let infer_nontrivial_pattern ?t_constr tenv (P.NPrecord (fields, closed_flag)) =
  W.map_l (infer_field ?t_constr tenv) (Simple.Record.to_list fields)
  >|= CCList.split >|= fun (envs, fields) ->
  let is_open = match closed_flag with
    | P.Open -> true
    | P.Closed -> false
  in
  (List.fold_left TE.merge TE.empty envs,
   Types.Builtins.record is_open @@ Simple.Record.of_list fields)


let infer_pattern_descr ?t_constr tenv p = match p with
  | P.Pvar (v, maybe_t) ->
    infer_var ?t_constr tenv maybe_t >|= fun t -> (TE.singleton v t, t)
  | P.Pnontrivial (sub_p, maybe_alias) ->
    infer_nontrivial_pattern ?t_constr tenv sub_p >|= fun (env, accepted_typ) ->
    (CCOpt.map_or
       ~default:env
       (fun var -> TE.add var accepted_typ env)
       maybe_alias,
     accepted_typ)

let infer_pattern ?t_constr tenv { L.description; _ } =
  infer_pattern_descr ?t_constr tenv description

let infer : ?t_constr:Types.t
  -> Types.Environment.t
  -> P.pattern
  -> (TE.t * Types.t) W.t =
  infer_pattern

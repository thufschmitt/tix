module P = Simple.Ast
module L = Parse.Location.With_loc
module TE = Typing_env

module W = Writer.Make(Warning.List)

let infer_pattern_descr ?t_constr tenv p = match p with
  | P.Pvar (v, maybe_t) ->
    let t_res =
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
    in
    W.map (fun t -> (TE.singleton v t, t)) t_res
  | _ -> failwith "TODO"

let infer_pattern ?t_constr tenv { L.description; _ } =
  infer_pattern_descr ?t_constr tenv description

let infer : ?t_constr:Types.t
  -> Types.Environment.t
  -> P.pattern
  -> (TE.t * Types.t) W.t =
  infer_pattern

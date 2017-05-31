module T = Typed_ast
module P = Simple.Ast
module TE = Typing_env

module L = Parse.Location.With_loc

let infer_pattern_descr tenv p = match p with
  | P.Pvar (v, maybe_t) ->
    let t =
      match CCOpt.fold
              (fun _ annot -> Annotations.to_type tenv annot)
              (Some Types.Builtins.any)
              maybe_t
      with
      | Some x -> x
      | None -> assert false
    in
    (TE.singleton v t, { T.With_type.description = T.Pvar v; typ = t; })
  | _ -> failwith "TODO"

let infer_pattern tenv { L.description; location } =
  let (env, descr) = infer_pattern_descr tenv description in
  (env, { L.description = descr; location })

let infer : Types.Environment.t -> P.pattern -> (TE.t * T.pattern) =
  infer_pattern

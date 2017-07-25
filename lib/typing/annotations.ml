(**
   Conversion of type annotations into actual types
*)

module A = Parse.Type_annotations
module L = Parse.Location
module T = Types

module W = Writer.Make (Warning.List)

let singleton =
  let module S = A.Singleton in
  function
  | S.Bool b -> W.pure @@ T.Singleton.bool b
  | S.Int  i -> W.pure @@ T.Singleton.int  i
  | S.String s -> W.pure @@
    Cduce_lib.(Types.atom (Atoms.atom @@ Atoms.V.mk_ascii s))

(* When typing recursive type-annotations, we need to keep an environment to
 * trace the local and yet undefined type variables.
 * Those can't be tracked in the global type environment as it is a map from
 * names to types, and we need a map from names to nodes (because we later
 * unify those types with their definition, and we only can do this with nodes.
 *
 * So we keep a separate environment.
 * *)
module Nodes_env = struct
  module M = CCMap.Make (CCString)

  type t = T.Node.t M.t

  let empty = M.empty

  let add = M.add

  let lookup t key = M.get key t
end

open W.Infix
let (<+>) = CCOpt.(<+>)
(* let (||>) opt msg = CCOpt.to_result msg opt *)
let (%>) = CCFun.(%>)

let rec to_node (nodes_env : Nodes_env.t) env (annot: A.t)
  : Cduce_lib.Types.Node.t W.t =
  let loc = Parse.Location.With_loc.loc annot in
  Parse.Location.With_loc.description annot |>
  function
  | A.Var v ->
    begin
      Nodes_env.lookup nodes_env v
      <+>
      (CCOpt.map T.node @@ Types.Environment.lookup env v)
      |> function
      | Some t -> W.pure t
      | None ->
        W.append
          [Warning.make loc ("Unbound type variable " ^ v)]
          (W.pure @@ T.node T.Builtins.grad)
    end
  | A.Infix (A.Infix_constructors.Arrow, t1, t2) ->
    W.map2
      T.Builtins.arrow
      (to_node nodes_env env t1)
      (to_node nodes_env env t2)
    >|= T.node
  | A.Infix (A.Infix_constructors.And, t1, t2) ->
    W.map2
      T.Builtins.cap
      (to_type nodes_env env t1)
      (to_type nodes_env env t2)
    >|= T.node
  | A.Infix (A.Infix_constructors.Or, t1, t2) ->
    W.map2
      T.Builtins.cup
      (to_type nodes_env env t1)
      (to_type nodes_env env t2)
    >|= T.node
  | A.Singleton s -> singleton s >|= T.node
  | A.TyBind (binds, t) ->
    let new_nodes_env, defs =
      List.fold_left
        (fun (env, defs) (name, def) ->
           let new_typ = T.fresh () in
           (Nodes_env.add name new_typ env,
            (new_typ, def) :: defs))
        (nodes_env, [])
        binds
    in
    begin
      let binds_errors =
        List.fold_left
          (fun accu (typ, def) ->
             let (type_def, errors) = to_type new_nodes_env env def in
             T.define typ type_def;
             errors @ accu
          )
          []
          defs
      in
      W.append
        binds_errors
        (to_node new_nodes_env env t)
    end
  | A.Cons (t1, t2) ->
    W.map2
      T.Builtins.cons
      (to_node nodes_env env t1)
      (to_node nodes_env env t2)
    >|= T.node
  | A.Gradual -> W.pure @@ T.node T.Builtins.grad

and to_type nodes_env env p = to_node nodes_env env p >|= T.typ

let to_type = to_type Nodes_env.empty

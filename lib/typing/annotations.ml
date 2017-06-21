(**
   Conversion of type annotations into actual types
*)

module A = Parse.Type_annotations
module T = Types
exception TypeError of string

let singleton =
  let module S = A.Singleton in
  function
  | S.Bool b -> CCResult.pure @@ T.Singleton.bool b
  | S.Int  i -> CCResult.pure @@ T.Singleton.int  i
  | S.String s -> CCResult.pure @@
    CCString.fold
      (fun accu char -> T.Builtins.cons
          (T.node @@ Cduce_lib.Types.char
             (Cduce_lib.Chars.(atom @@ V.mk_char char)))
          (T.node accu))
      T.Builtins.nil
      (CCString.rev s)

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

let (>|=) = CCResult.(>|=)
let (<+>) = CCOpt.(<+>)
let (|>) opt msg = CCOpt.to_result msg opt
let (%>) = CCFun.(%>)

let rec to_node (nodes_env : Nodes_env.t) env
  : A.t -> (Cduce_lib.Types.Node.t, string) result =
  Parse.Location.With_loc.description %>
  function
  | A.Var v ->
    Nodes_env.lookup nodes_env v
    <+>
    (CCOpt.map T.node @@ Types.Environment.lookup env v)
    |> ("Unbound variable " ^ v)
  | A.Infix (A.Infix_constructors.Arrow, t1, t2) ->
    CCResult.both
      (to_node nodes_env env t1)
      (to_node nodes_env env t2)
    >|= CCFun.uncurry T.Builtins.arrow
    >|= T.node
  | A.Infix (A.Infix_constructors.And, t1, t2) ->
    CCResult.both
      (to_type nodes_env env t1)
      (to_type nodes_env env t2)
    >|= CCFun.uncurry T.Builtins.cap
    >|= T.node
  | A.Infix (A.Infix_constructors.Or, t1, t2) ->
    CCResult.both
      (to_type nodes_env env t1)
      (to_type nodes_env env t2)
    >|= CCFun.uncurry T.Builtins.cup
    >|= T.node
  | A.Singleton s -> singleton s
    >|= T.node
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
      try
        List.iter
          (fun (typ, def) ->
             let type_def =
               CCResult.catch
                 ~ok:CCFun.id
                 ~err:(fun e -> raise (TypeError e))
                 (to_type new_nodes_env env def)
             in
             T.define typ type_def)
          defs;
        to_node new_nodes_env env t
      with TypeError e -> Error e
    end
  | A.Cons (t1, t2) ->
    CCResult.both
      (to_node nodes_env env t1)
      (to_node nodes_env env t2)
    >|= CCFun.uncurry T.Builtins.cons
    >|= T.node

and to_type nodes_env env p = to_node nodes_env env p >|= T.typ

let to_type = to_type Nodes_env.empty

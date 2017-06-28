(** Definition of the types used by Onix
    This is mostly a wrapper around Cduce types.
*)

module C = Cduce_lib
module T = Cduce_lib.Types

type t = T.t
let pp = T.Print.pp_type
let show = T.Print.string_of_type

let node = T.cons
let typ  = T.descr

let sub   = T.subtype
let equiv = T.equiv

(** Creates a fresh new node *)
let fresh = T.make

(** [unify t1 t2] adds the equation [t1 = t2] to the environment *)
let define = T.define

module Intervals : sig
  include module type of C.Intervals

  val singleton_of_int : int -> t
end = struct
  include C.Intervals

  let singleton i = bounded i i
  let singleton_of_int i =
    let i = C.Intervals.V.from_int i in
    singleton i
end

module Node = struct
  type t = T.Node.t
end

(** Builtin types *)
module Builtins : sig
  val true_type : t (* [true] is a keyword in OCaml *)
  val false_type : t (* [false] is a keyword in OCaml *)

  val int : t
  val bool : t
  val char : t
  val string : t
  val nil : t
  val any : t
  val empty : t
  val grad : t
  val undef : t

  val interval : Intervals.t -> t

  val cons  : Node.t -> Node.t -> t
  val arrow : Node.t -> Node.t -> t
  val cup   : t -> t -> t
  val cap   : t -> t -> t
  val neg   : t -> t
end
= struct
  include C.Builtin_defs

  let empty = T.empty

  (* TODO: find a cleaner way to define this *)
  let grad = T.atom (C.Atoms.(atom @@ V.mk_ascii "?"))

  let undef = T.atom (C.Atoms.(atom @@ V.mk_ascii "%%undef"))

  let interval = C.Types.interval

  let arrow = C.Types.arrow

  let cons  = C.Types.times

  let cup = C.Types.cup
  let cap = C.Types.cap
  let neg = C.Types.neg
end

module Singleton = struct
  let int i = C.(Types.interval Intervals.(bounded
                                             (V.from_int i)
                                             (V.from_int i)))

  let bool = function
    | true -> C.Builtin_defs.true_type
    | false -> C.Builtin_defs.false_type
end

module Environment : sig
  (** The type representing a type environment.
      A type environment is a map from type variables to their definition
  *)
  type t

  (** The empty environment *)
  val empty : t

  (** The default environment containing all the builtin types *)
  val default : t

  val lookup : t -> string -> T.t option

  (** [add nam typ env] adds a new type named [nam] and defined by [typ] to the
      environment [env], relpacing any previously defined typ with that name
   * *)
  val add : string -> T.t -> t -> t
end = struct
  module M = CCMap.Make(String)
  type t = T.t M.t

  let empty = M.empty

  let builtin_types =
    let module B = Builtins in
    [
      "Int", B.int;
      "Bool", B.bool;
      "Char", B.char;
      "String", B.string;
      "true", B.true_type;
      "false", B.false_type;
      "?", B.grad;
      "nil", B.nil;
    ]

  let default =
    M.of_list builtin_types

  let lookup env name = M.get name env

  let add = M.add
end

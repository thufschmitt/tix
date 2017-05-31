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

  val interval : Intervals.t -> t

  val arrow : t -> t -> t
  val cup   : t -> t -> t
  val cap   : t -> t -> t
end
= struct
  include C.Builtin_defs

  let empty = T.empty

  (* TODO: find a cleaner way to define this *)
  let grad = T.atom (C.Atoms.(atom @@ V.mk_ascii "?"))

  let interval = C.Types.interval

  let arrow t1 t2 = C.Types.arrow (node t1) (node t2)

  let cup = C.Types.cup
  let cap = C.Types.cap
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
      "?", B.grad
    ]

  let default =
    M.of_list builtin_types

  let lookup env name = M.get name env
end

(** Definition of the types used by Onix
    This is mostly a wrapper around Cduce types.
*)

module C = Cduce_lib
module T = Cduce_lib.Types

type t = T.t

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

  val interval : Intervals.t -> t
end
= struct
  include C.Builtin_defs

  let interval = C.Types.interval
end

module Environment : sig
  (** The type representing a typing environment.
      Once again, stolen from Cduce.
  *)
  type t

  (** The empty environment *)
  val empty : t

  (** The default environment containing all the builtin types *)
  val default : t

  val lookup : t -> string -> T.t
end = struct
  type t = Cduce_lib.Typer.t

  let empty = Cduce_lib.Typer.empty_env

  let builtin_types =
    let module B = Builtins in
    [
      "Int", B.int;
      "Bool", B.bool;
      "Char", B.char;
      "String", B.string;
      "true", B.true_type;
      "false", B.false_type;
    ]

  let default =
    List.fold_left
      (fun acc (name, typ) ->
         let n = (C.Ns.empty, C.Ident.U.mk name) in
         T.Print.register_global ("", n, []) typ;
         C.Typer.enter_type (C.Ident.ident n) typ acc)
      empty
      builtin_types

  let lookup env (name : string) =
    Cduce_lib.Typer.find_value
      (C.Ns.empty, C.Encodings.Utf8.mk name)
      env
end

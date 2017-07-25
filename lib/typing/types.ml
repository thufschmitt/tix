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

let lift (direction : [> `Up | `Down ]) t =
  let reverse = function `Up -> `Down | `Down -> `Up in
  let replace_gradual direction =
    match direction with
    | `Up -> T.any
    | `Down -> T.empty
  in
  let map_bdd (type a b) (module S : C.Bool.S
                           with type elem = a
                            and type t = b) atm =
    let open T in
    S.compute ~empty ~full:any ~cup ~cap ~diff ~atom:atm
  in
  let map_vartype (type a) (module V : T.VarType with type Atom.t = a)
      direction atm t =
    map_bdd (module V) (function
        | `Var v when C.Var.equal v (C.Var.mk "?") ->
          replace_gradual direction
        | `Var _ -> assert false
        | `Atm a -> atm a)
      (V.proj t)
  in
  let rec replace_gradual (direction : [> `Up | `Down ]) (typ : t) : t =
    let atoms = map_vartype (module T.VarAtoms) direction T.atom
    and ints = map_vartype (module T.VarIntervals) direction T.interval
    and chars = map_vartype (module T.VarChars) direction T.char
    and times = map_vartype (module T.VarTimes) direction
        (map_bdd (module T.Pair) @@ fun (n1, n2) ->
         T.times
           (replace_gradual_node direction n1)
           (replace_gradual_node direction n2))
    (* and xml = Don't care, we don't use xml types *)
    and arrows = map_vartype (module T.VarArrow) direction
        (map_bdd (module T.Pair) @@ fun (n1, n2) ->
         T.arrow
           (replace_gradual_node (reverse direction) n1)
           (replace_gradual_node direction n2))
    and records = map_vartype (module T.VarRec) direction
        (map_bdd (module T.Rec) @@ fun r ->
         CCPair.map2 (C.Ident.LabelMap.map
                        (replace_gradual_node direction)
                     ) r
         |> T.record_fields)
    and abstracts = map_vartype (module T.VarAbstracts) direction T.abstract
    in
    List.fold_left T.cup T.empty @@
    List.map (fun f -> f typ)
      [arrows; ints; atoms; chars; times; records; abstracts]
  and replace_gradual_node direction n =
    T.cons @@ replace_gradual direction (T.descr n)
  in
  replace_gradual direction t

let sub t1 t2 =
  C.Type_tallying.is_squaresubtype C.Var.Set.empty
    (lift `Down t1)
    (lift `Up t2)

let applicative_lift t =
  assert (sub t T.Arrow.any);
  T.Iter.compute
    ~default:T.empty
    ~cup:(List.fold_left T.cup T.empty)
    ~cap:(List.fold_left T.cap T.any)
    ~neg:T.neg
    ~var:(fun v ->
        if C.Var.equal v (C.Var.mk "?") then
          T.arrow (T.cons @@ T.any) (T.cons @@ T.var v)
        else assert false)
    ~arrow:(fun (n1, n2) ->
        T.arrow
          (T.cons @@ (lift `Up (T.descr n1)))
          n2)
    t

let get_arrow t = applicative_lift t |> T.Arrow.get
let arrow_apply arrow arg =
  T.Arrow.apply arrow (T.cap (T.Arrow.domain arrow) (lift `Up arg))

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

module String = struct
  module StrSet = CCSet.Make(CCString)

  let str_ns = C.Ns.Uri.mk @@ C.Encodings.Utf8.mk "str"

  let any = T.atom @@ C.Atoms.any_in_ns str_ns

  let singleton s =
    T.atom @@ C.Atoms.atom (C.Atoms.V.mk (str_ns, C.Encodings.Utf8.mk s))

  (** [get t] returns either [`Finite l] where [l] is the list of the strings
      that the type [t] contains or [`Infinite].
      No check is done to ensure that [t] is a subtype of [string] *)
  let get t : [> `Finite of string list | `Infinite ] =
    let atoms = T.Atom.get t in
    let cup x1 x2 = match (x1, x2) with
      | `Finite l1, `Finite l2 -> `Finite (StrSet.union l1 l2)
      | `Cofinite l1, `Cofinite l2 -> `Cofinite (StrSet.inter l1 l2)
      | `Cofinite l1, `Finite l2 -> `Cofinite (StrSet.diff l1 l2)
      | `Finite l1, `Cofinite l2 -> `Cofinite (StrSet.diff l2 l1)
      | `Variable, _
      | _, `Variable -> `Variable
    and neg = function
      | `Finite l -> `Cofinite l
      | `Cofinite l -> `Finite l
      | `Variable -> `Variable
    in
    let cap x1 x2 = neg @@ cup (neg x1) (neg x2) in
    let diff x1 x2 = cap x1 (neg x2) in
    let get_from_atom atm =
      let get_name atom_elt =
        let (_, unicode_name) = C.Atoms.V.value atom_elt in
        C.Encodings.Utf8.get_str unicode_name
      in
      let (direction, sub_atoms) =
        match C.Atoms.extract atm with
        | `Finite s -> (`Finite, s)
        | `Cofinite s -> (`Cofinite, s)
      in
      let string_atoms = CCList.find_map
          (fun (ns, atms) ->
             if C.Ns.Uri.equal str_ns ns then Some atms else None)
          sub_atoms
                         |> CCOpt.get_or ~default:(`Finite [])
      in
      begin match string_atoms with
        | `Finite elts -> `Finite (StrSet.of_list (List.map get_name elts))
        | `Cofinite elts -> `Cofinite (StrSet.of_list (List.map get_name elts))
      end
      |> (fun s -> if direction = `Cofinite then neg s else s)
    in
    match
      T.VarAtoms.compute atoms
        ~atom:(function `Var _ -> `Variable | `Atm a -> get_from_atom a)
        ~empty:(`Finite StrSet.empty)
        ~full:(`Cofinite StrSet.empty)
        ~cup
        ~cap
        ~diff
    with
    | `Cofinite _ -> `Infinite
    | `Variable -> `Infinite
    | `Finite l -> `Finite (StrSet.to_list l)
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
  val diff  : t -> t -> t
  val neg   : t -> t
  val record : bool -> Node.t Simple.Record.t -> t
end
= struct
  include C.Builtin_defs

  let empty = T.empty

  let grad = T.var (C.Var.mk "?")

  let undef = T.atom (C.Atoms.(atom @@ V.mk_ascii "%%undef"))

  let interval = C.Types.interval

  (* We don't use CDuce's strings because these are lists of chars (which isn't
     the case in Nix) *)
  let string = String.any

  let arrow = C.Types.arrow

  let cons  = C.Types.times

  let cup = C.Types.cup
  let cap = C.Types.cap
  let diff = C.Types.diff
  let neg = C.Types.neg

  let record is_open fields =
    let label_fields =
      fields
      |> Simple.Record.to_list
      |> List.map (fun (key, value) ->
          (Cduce_lib.Ns.Label.mk_ascii key, value))
      |> Cduce_lib.Ident.LabelMap.from_list_disj
    in
    Cduce_lib.Types.record_fields (is_open, label_fields)
end

module Singleton = struct
  let int i = C.(Types.interval Intervals.(bounded
                                             (V.from_int i)
                                             (V.from_int i)))

  let bool = function
    | true -> C.Builtin_defs.true_type
    | false -> C.Builtin_defs.false_type

  let string = String.singleton
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
  module M = CCMap.Make(CCString)
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
      "%%undef", B.undef;
      "Empty", B.empty;
      "Any", B.any;
    ]

  let default =
    M.of_list builtin_types

  let lookup env name = M.get name env

  let add = M.add
end

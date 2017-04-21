type 'a with_loc = 'a Onix_location.with_loc [@@deriving show]

type operator =
  | Ocons
  [@@deriving show]

type expr = expr_desc with_loc

and expr_desc =
  | Evar of string
  | EaccessPath of access_path
  (*
   * x
   * x.y
   * x.y or e
   * *)
  | Econstant of constant
  | Elambda of lambda
  | EfunApp of expr * expr
  | EopApp of operator * expr list
  | Elist of expr list
  | Erecord of record
  | Ewith of expr * expr
  (* with e; e *)
  | Elet of binding list * expr
  | EtyAnnot of (expr * Tix_types.t)

and access_path =
  | Afield of expr * ap_field * expr option
  (* e.f or e' *)

and ap_field = expr with_loc

and constant =
  | Cint of int
  | Cbool of bool
  | Cstring of string
  | Cnil

and lambda = pattern * expr * Tix_types.t option

and pattern = pattern_desc with_loc

and pattern_desc =
  | Pvar of string
  | Pnontrivial of nontrivial_pattern
  | Paliased of nontrivial_pattern * string

and nontrivial_pattern =
  | NPrecord of (string * expr option) list * closed_flag * string option
  (* fields * '...' * @a *)

and closed_flag =
  | Closed
  | Open

and record = {
  recursive : bool;
  fields : (field with_loc) list;
}

and field =
  | Fdef of expr * expr

and binding =
  | Bdef of string * expr

and interpol = expr
[@@deriving show]


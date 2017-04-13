type 'a with_loc = 'a Onix_location.with_loc [@@deriving show]

type expression = expression_desc with_loc

and expression_desc =
  | Var of string
  | Access_path of access_path
  (*
   * x
   * x.y
   * x.y or e
   * *)
  | Constant of constant
  | String of str
  (* Not constant because of interpolation *)
  | Lambda of lambda
  | Fun_app of expression * expression
  | List of expression list
  | Record of record
  | With of expression * expression
  (* with e; e *)
  | Let of binding list * expression

and access_path =
  | Ap_field of expression * ap_field * expression option
  (* e.f or e' *)

and ap_field = ap_field_desc with_loc

and ap_field_desc =
  | Fdesc_identifier of string
  | Fdesc_string of str
  | Fdesc_interpol of interpol

and constant =
  | Cst_int of int
  | Cst_path of string
  | Cst_url of string

and str = (str_element with_loc) list

and str_element =
  | Str_constant of string
  | Str_interpol of interpol

and lambda = pattern * expression

and pattern = pattern_desc with_loc

and pattern_desc =
  | Pvar of string
  | Pnontrivial of nontrivial_pattern
  | Paliased of nontrivial_pattern * string

and nontrivial_pattern =
  | Precord of (string * expression option) list * closed_flag * string option
  (* fields * '...' * @a *)

and closed_flag =
  | Closed
  | Open

and record = {
  recursive : bool;
  fields : (field with_loc) list;
}

and field =
  | Field_definition of access_path * expression
  | Field_simple_definition of string * expression
  | Inherit of inherit_
  (* inherit x y z...;
   * inherit (e) x y z...;
   *)

and binding =
  | Bdef of access_path * expression
  | Binherit of inherit_

and inherit_ = expression option * (string with_loc) list

and interpol = expression
[@@deriving show]

let _ = show_expression

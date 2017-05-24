(**
   Parsetree for the nix language
 *)

type 'a with_loc = 'a Location.With_loc.t

type operator =
  | Ocons
 

type expr = expr_desc with_loc

and expr_desc =
  | Evar of string
  | EaccessPath of access_path
  (**
     x
     x.y
     x.y or e
   *)
  | Econstant of constant
  | Estring of str
  (* Not constant because of interpolation *)
  | Elambda of lambda
  | EfunApp of expr * expr
  | EopApp of operator * expr list
  | Erecord of record
  | Ewith of expr * expr
  (** with e; e *)
  | Elet of binding list * expr
  | EtyAnnot of expr * Tix_types.t

and access_path =
  | Afield of expr * ap_field * expr option
  (** e.f or e' *)

and ap_field = ap_field_desc with_loc

and ap_field_desc =
  | AFidentifier of string
  | AFstring of str
  | AFinterpol of interpol

and constant =
  | Cint of int
  | Cbool of bool
  | Cnil

and str = (str_element with_loc) list

and str_element =
  | Sconstant of string
  | Sinterpol of interpol

and lambda = pattern * expr

and pattern = pattern_desc with_loc

and pattern_desc =
  | Pvar of string * Tix_types.t option
  | Pnontrivial of nontrivial_pattern * string option

and nontrivial_pattern =
  | NPrecord of pattern_record_field list * closed_flag
  (* fields * '...' *)

and pattern_record_field = {
  field_name: string;
  default_value: expr option;
  type_annot: Tix_types.t option;
}

and closed_flag =
  | Closed
  | Open

and record = {
  recursive : bool;
  fields : (field with_loc) list;
}

and field =
  | Fdef of string * expr
  | FstaticDef of string * expr
  | Finherit of inherit_
  (** inherit x y z...;
     inherit (e) x y z...;
   *)

and binding =
  | Bdef of access_path * expr (* FIXME: the first element can not be an arbitrary expr *)
  | BstaticDef of string * expr
  | Binherit of inherit_

and inherit_ = expr option * (string with_loc) list

and interpol = expr


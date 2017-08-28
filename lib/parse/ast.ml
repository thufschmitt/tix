(**
   Parsetree for the nix language
*)

module Type_annotations = Common.Type_annotations

type 'a with_loc = 'a Common.Location.With_loc.t

type binop =
  | Ocons
  | Oeq | OnonEq
  | Oplus | Ominus
  | Oand | Oor | Oimplies
  | Omerge
  | Oconcat

type monop =
  | Oneg
  | Onot

type expr = expr_desc with_loc

and expr_desc =
  | Evar of string
  | Eaccess of  expr * access_path * expr option
  (**
     e
     e.y
     e.y or e
  *)
  | Econstant of constant
  | Elambda of lambda
  | EfunApp of expr * expr
  | Ebinop of binop * expr * expr
  | Emonop of monop * expr
  | Eite of expr * expr * expr
  (* if e then e else e *)
  | Erecord of record
  | Ewith of expr * expr
  (** with e; e *)
  | Elet of binding list * expr
  | EtyAnnot of expr * Type_annotations.t
  | Epragma of Pragma.t * expr
  | EtestMember of expr * access_path

and access_path = ap_field list

and ap_field = ap_field_desc with_loc

and ap_field_desc =
  | AFidentifier of string
  | AFexpr of expr

and constant =
  | Cint of int
  | Cbool of bool
  | Cstring of string
  | Cpath of string
  | Cbracketed of string
  (* A bracketed path, which will be of type `Path` *)

and lambda = pattern * expr

and pattern = pattern_desc with_loc

and pattern_desc =
  | Pvar of pattern_var
  | Pnontrivial of nontrivial_pattern * string option

and nontrivial_pattern =
  | NPrecord of pattern_record_field list * closed_flag
  (* fields * '...' *)

and pattern_record_field = {
  field_name: string;
  default_value: expr option;
  type_annot: Type_annotations.t option;
}

and pattern_var = string * Type_annotations.t option

and pattern_access_path = access_path * Type_annotations.t option

and closed_flag =
  | Closed
  | Open

and record = {
  recursive : bool;
  fields : field list;
}

and field_desc =
  | Fdef of pattern_access_path * expr
  | Finherit of inherit_
  (** inherit x y z...;
      inherit (e) x y z...;
  *)

and field = field_desc with_loc

and binding = field

and inherit_ = expr option * (string with_loc) list

and interpol = expr

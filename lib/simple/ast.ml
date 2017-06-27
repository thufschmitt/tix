(**
   AST for nix-light, a simplified version of the nix language
   @see <https://github.com/regnat/tix-papers> for a description of the language
*)
type 'a with_loc = 'a Parse.Location.With_loc.t

type operator =
  | Ocons
  | Oeq
  | Oneg
  | Oplus
  | Ominus

type expr = expr_desc with_loc

and expr_desc =
  | Evar of string
  | EaccessPath of expr * access_path * expr option
  (**
     x
     x.y
     x.y or e
  *)
  | Econstant of constant
  | Elambda of lambda
  | EfunApp of expr * expr
  | EopApp of operator * expr list
  | Eite of expr * expr * expr
  | Erecord of record
  | Ewith of expr * expr
  (* with e; e *)
  | Elet of binding list * expr
  | EtyAnnot of expr * Parse.Type_annotations.t
  | Epragma of Parse.Pragma.t * expr
  | Eimport of expr

and access_path = ap_field list

and ap_field = expr

and constant =
  | Cint of int
  | Cbool of bool
  | Cstring of string

and lambda = pattern * expr

and pattern = pattern_desc with_loc

and pattern_desc =
  | Pvar of pattern_var
  | Pnontrivial of nontrivial_pattern * string option

and nontrivial_pattern =
  | NPrecord of pattern_record_field list * closed_flag
  (** fields * '...' *)

and pattern_var = string * Parse.Type_annotations.t option

and pattern_record_field = {
  field_name: string;
  default_value: constant option;
  type_annot: Parse.Type_annotations.t option;
}

and closed_flag =
  | Closed
  | Open

and record = (field with_loc) list

and field = expr * expr

and binding = pattern_var * expr

and interpol = expr

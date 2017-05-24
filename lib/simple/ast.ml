(**
 AST for nix-light, a simplified version of the nix language
 @see <https://github.com/regnat/tix-papers> for a description of the language
 *)
type 'a with_loc = 'a Parse.Location.With_loc.t

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
  | Elambda of lambda
  | EfunApp of expr * expr
  | EopApp of operator * expr list
  | Erecord of record
  | Ewith of expr * expr
  (* with e; e *)
  | Elet of binding list * expr
  | EtyAnnot of (expr * Tix_types.t)

and access_path =
  | Afield of expr * ap_field * expr option
  (** e.f or e' *)

and ap_field = expr with_loc

and constant =
  | Cint of int
  | Cbool of bool
  | Cstring of string
  | Cnil

and lambda = pattern * expr

and pattern = pattern_desc with_loc

and pattern_desc =
  | Pvar of string * Tix_types.t option
  | Pnontrivial of nontrivial_pattern * string option

and nontrivial_pattern =
  | NPrecord of pattern_record_field list * closed_flag
  (** fields * '...' *)

and pattern_record_field = {
  field_name: string;
  default_value: constant option;
  type_annot: Tix_types.t option;
}

and closed_flag =
  | Closed
  | Open

and record = (field with_loc) list

and field = expr * expr

and binding = string * expr

and interpol = expr


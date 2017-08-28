(**
   AST for nix-light, a simplified version of the nix language
   @see <https://github.com/regnat/tix-papers> for a description of the language
*)

module Type_annotations = Common.Type_annotations

type 'a with_loc = 'a Common.Location.With_loc.t

type binop =
  | Ocons
  | Oeq
  | Oplus
  | Ominus
  | Oand | Oor
  | OrecordMember
  | Omerge
  | Oconcat

type monop =  Oneg| Onot 

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
  | Ebinop of binop * expr * expr
  | Emonop of monop * expr
  | Eite of expr * expr * expr
  | Erecord of record
  | Ewith of expr * expr
  (* with e; e *)
  | Elet of binding list * expr
  | EtyAnnot of expr * Type_annotations.t
  | Epragma of Parse.Pragma.t * expr
  | Eimport of expr

and access_path = ap_field list

and ap_field = expr

and constant =
  | Cint of int
  | Cbool of bool
  | Cstring of string
  | Cpath of string
  | Cbracketed of string
  | Cundef (* Used for pattern-matching against records with optinal fields *)

and lambda = pattern * expr

and pattern = pattern_desc with_loc

and pattern_desc =
  | Pvar of pattern_var
  | Pnontrivial of nontrivial_pattern * string option

and nontrivial_pattern =
  | NPrecord of (bool * Type_annotations.t option) Record.t * closed_flag
  (** fields * '...'
   * For each field, the boolean indicates whether the field is optional
   * *)

and pattern_var = string * Type_annotations.t option

and closed_flag =
  | Closed
  | Open

and record = field list

and field = expr * Type_annotations.t option * expr

and binding = pattern_var * expr

and interpol = expr

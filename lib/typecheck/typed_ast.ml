(**
   The nix-light AST with type informations
*)
module P = Nl_ast
module T = Tix_types

type 'a with_loc = 'a Onix_location.With_loc.t [@@deriving show]

module With_type =
struct
  type 'a t = {
    description: 'a;
    typ: T.t;
  } [@@deriving show, make]

  let typ x = x.typ
  let description x = x.description
end

type 'a with_type = 'a With_type.t
  [@@deriving show]

type operator =
  | Ocons
  [@@deriving show]

type expr = expr_desc with_type with_loc

and expr_desc =
  | Evar of string
  | Econstant of P.constant
  | Elambda of lambda
  | EfunApp of expr * expr
  | EopApp of operator with_type * expr list

and lambda = pattern * expr

and pattern = pattern_desc with_type with_loc

and pattern_desc =
  | Pvar of string
  [@@deriving show]

let get_typ x = With_type.typ x.Onix_location.With_loc.description
let get_descr x = With_type.description x.Onix_location.With_loc.description

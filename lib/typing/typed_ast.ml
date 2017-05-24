(**
   The nix-light AST with type informations
*)
module P = Simple.Ast
module T = Type_annotations

type 'a with_loc = 'a Parse.Location.With_loc.t

module With_type =
struct
  type 'a t = {
    description: 'a;
    typ: T.t;
  }

  let typ x = x.typ
  let description x = x.description

  let make ~description ~typ = { description; typ }
end

type 'a with_type = 'a With_type.t

type operator =
  | Ocons

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

let get_typ x = With_type.typ x.Parse.Location.With_loc.description
let get_descr x = With_type.description x.Parse.Location.With_loc.description

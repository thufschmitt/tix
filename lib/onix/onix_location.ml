(**
   Simple data structure to add location information to AST nodes
 *)
type t = {
  pos_start: Lexing.position [@opaque];
  pos_end: Lexing.position [@opaque];
} [@@deriving show]

let mk pos_start pos_end = {
  pos_start;
  pos_end;
}

module With_loc =
struct
  (* Nonrec types seem to mess with ppx_deriving, so let's not use it *)
  type 'a _t = {
    description: 'a;
    location: t;
  } [@@deriving show]

  type 'a t = 'a _t [@@deriving show]

  let mk pos_start pos_end elt = {
    description = elt;
    location = mk pos_start pos_end;
  }

  let map f x = {
    x with
    description = f x.description;
  }

  let description { description = it; _ } = it
end

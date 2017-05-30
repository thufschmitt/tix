(**
   Simple data structure to add location information to AST nodes
 *)
type t = {
  pos_start: Lexing.position [@opaque];
  pos_end: Lexing.position [@opaque];
}

let mk pos_start pos_end = {
  pos_start;
  pos_end;
}

let pp_position fmt pos =
  let open Lexing in
  Format.fprintf fmt "[%s:%d:%d]"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)

let pp fmt t = pp_position fmt t.pos_start

module With_loc =
struct
  (* Nonrec types seem to mess with ppx_deriving, so let's not use it *)
  type 'a _t = {
    description: 'a;
    location: t;
  }

  type 'a t = 'a _t

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

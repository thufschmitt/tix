(**
   Simple data structure to add location information to AST nodes
*)
type t = {
  file_name: string;
  lnum: int;
  cnum: int;
}

let mk ?(file_name = "") ?(lnum = -1) ?(cnum = -1) () = {
  file_name;
  lnum;
  cnum;
}

let pp fmt t =
  Format.fprintf fmt "file %s, line %i, character %i"
    t.file_name
    t.lnum
    t.cnum

module With_loc =
struct
  (* Nonrec types seem to mess with ppx_deriving, so let's not use it *)
  type 'a _t = {
    description: 'a;
    location: t;
  }

  type 'a t = 'a _t

  let mk' ?file_name ?lnum ?cnum elt = {
    description = elt;
    location = mk ?file_name ?lnum ?cnum ()
  }

  let mk location description = { location; description; }

  let map f x = {
    x with
    description = f x.description;
  }

  let description { description = it; _ } = it
  let loc { location = l; _ } = l
end

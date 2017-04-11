type t = {
  pos_start: Lexing.position [@opaque];
  pos_end: Lexing.position [@opaque];
} [@@deriving show]

type 'a with_loc = {
  description: 'a;
  location: t;
} [@@deriving show]

let mk_loc pos_start pos_end = {
  pos_start;
  pos_end;
}

let mk_with_loc pos_start pos_end elt = {
  description = elt;
  location = mk_loc pos_start pos_end;
}

type kind = Error | Warning

let show_kind = function
  | Error -> "error"
  | Warning -> "warning"

let pp_kind fmt k = Format.pp_print_string fmt (show_kind k)

type t = {
  location : Location.t;
  kind: kind;
  error: string;
}

let make ?(kind=Error) location error = { error; location;  kind}

let format location f = Format.ksprintf (make location) f

let pp fmt t =
  Format.fprintf fmt
    "%a: %s at %a"
    pp_kind t.kind
    t.error
    Location.pp t.location

let show t =
  pp Format.str_formatter t;
  Format.flush_str_formatter ()

let get_kind t = t.kind

module List = struct
  type nonrec t = t list

  let empty : t = []
  let append : t -> t -> t = CCList.append

  let contains_error =
    CCList.exists (fun w -> w.kind = Error)
end

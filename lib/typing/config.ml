module Warnings = struct
  module Warning = struct
    type t =
      | TypeError

    let compare (a: t) b = Pervasives.compare a b

    let show = function
      | TypeError -> "TypeError"

    let read = function
      | "TypeError" -> TypeError
      | _ -> raise (Invalid_argument "Config.Warnings.Warning.read")
  end
  module WSet = CCSet.Make (Warning)

  type t = WSet.t

  let default = WSet.of_list [ Warning.TypeError ]

  let proceed_annot warns (sign, warning_name) =
    let warning = Warning.read warning_name in
    match sign with
    | Parse.Pragma.Plus -> WSet.add warning warns
    | Parse.Pragma.Minus -> WSet.remove warning warns

  let proceed_annots = List.fold_left proceed_annot
end

type t = {
  warnings: Warnings.t;
}

let default = { warnings = Warnings.default; }

let map_warnings f t = { warnings = f t.warnings; }

let proceed_warnings_annot t annot =
  map_warnings (fun w -> Warnings.proceed_annots w annot) t

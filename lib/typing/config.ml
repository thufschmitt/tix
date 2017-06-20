module WSet = struct
  include  CCSet.Make (Parse.Pragma.Warning)

  let default = empty
  let default_err = of_list [ Parse.Pragma.Warning.TypeError ]

  let proceed_annot warns (sign, warning_name) =
    let warning = Parse.Pragma.Warning.read warning_name in
    match sign with
    | Parse.Pragma.Plus -> add warning warns
    | Parse.Pragma.Minus -> remove warning warns

  let proceed_annots = List.fold_left proceed_annot
end

type t = {
  warnings: WSet.t;
  errors: WSet.t;
}

let default = {
  warnings = WSet.default;
  errors = WSet.default_err;
}

let map_warnings f t = { t with warnings = f t.warnings; }
let map_errors f t = { t with errors = f t.errors; }

let proceed_warnings_annot t annot =
  map_warnings (fun w -> WSet.proceed_annots w annot) t
let proceed_errors_annot t annot =
  map_errors (fun w -> WSet.proceed_annots w annot) t

module Warnings = struct
  module Warning = struct
    type t =
      | TypeError

    let compare (a: t) b = Pervasives.compare a b

    let show = function
      | TypeError -> "TypeError"
  end
  module WSet = CCSet.Make (Warning)

  type t = WSet.t

  let default = WSet.of_list [ Warning.TypeError ]
end

type t = {
  warnings: Warnings.t;
}

let default = { warnings = Warnings.default; }

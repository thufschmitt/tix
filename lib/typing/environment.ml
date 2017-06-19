module C = Config
module TE = Types.Environment
module VE = Typing_env

type t = {
  types: TE.t;
  values: VE.t;
  config: C.t;
}

let default = {
  types = TE.default;
  values = VE.initial;
  config = C.default;
}

let map_values f e = {
  e with
  values = f e.values;
}

let add_values e new_values =
  map_values (fun v -> VE.merge v new_values) e

let add_value e name value =
  map_values (fun v -> VE.add name value v) e

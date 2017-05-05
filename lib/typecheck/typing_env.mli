type t

val empty : t

val singleton : string -> Tix_types.t -> t

val add : string -> Tix_types.t -> t -> t

val merge : t -> t -> t

val lookup : t -> string -> Tix_types.t option

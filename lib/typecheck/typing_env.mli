type t

val empty : t

val add : string -> Tix_types.t -> t -> t

val lookup : t -> string -> Tix_types.t

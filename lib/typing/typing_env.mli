(**
   {2 Typing environments}
 *)

(**
   A typing environment Γ
 *)
type t

(**
   The empty env
 *)
val empty : t

(**
   [singleton x τ] is the environment containing the only constraint {e x: τ}
 *)
val singleton : string -> Type_annotations.t -> t

(**
   [add x τ Γ] returns the environment {e Γ; x:τ}
 *)
val add : string -> Type_annotations.t -> t -> t

(**
   [merge Γ Γ'] is the environment {e Γ; Γ'}
 *)
val merge : t -> t -> t

(**
   [lookup Γ x] returns the type constraint associated to [x] in [Γ], on [None]
   if [x] isn't in [Γ]
 *)
val lookup : t -> string -> Type_annotations.t option

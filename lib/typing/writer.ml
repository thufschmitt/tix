module type MONOID = sig
  type t

  val empty : t

  val append : t -> t -> t
end

module type S = sig
  type log
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val pure : 'a -> 'a t
  val ap : ('a -> 'b) t -> 'a t -> 'b t

  val return : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t

  val join : ('a t) t -> 'a t

  val iter : ('a -> unit) -> 'a t -> unit

  val append : log -> 'a t -> 'a t
  val value : 'a t -> 'a
  val log : 'a t -> log

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

  val map_l  : ('a -> 'b t) -> 'a list -> 'b list t
  val iter_l : ('a -> unit t) -> 'a list -> unit t

  module Infix : sig
    val (<$>) : ('a -> 'b) -> 'a t -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t

    val (<*>) : 'a t -> ('a -> 'b) t -> 'b t

    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>>)  : 'a t -> 'b t -> 'b t
  end
end

module Make(M: MONOID) : S with type 'a t = ('a * M.t) and type log = M.t =
struct
  type log = M.t
  type 'a t = ('a * M.t)

  (* Functor *)
  let map f (x, log) = (f x, log)

  (* Applicative *)
  let pure x = (x, M.empty)
  let ap (f, log) (x, log') =
    (f x, M.append log log')

  (* Monad *)
  let return x = pure x
  let bind f (x, log) =
    let (x', log') = f x in
    (x', M.append log' log)

  let join ((x, log), log') = (x, M.append log log')

  let iter f x = ignore (map f x)

  let append log (x, log') = (x, M.append log' log)

  let value = fst
  let log = snd

  let map2 f (x, log) (y, log') = (f x y, M.append log log')

  let map_l f l =
    CCList.map f l
    |> List.fold_left (fun accu (elt, log) ->
        bind (fun partial_list -> (elt :: partial_list, log)) accu)
      (pure [])

  let iter_l f l =
    map (fun _ -> ()) (map_l f l)

  module Infix = struct
    let (<$>) = map
    let (>|=) x f = map f x

    let (<*>) x f = ap f x

    let (>>=) x f = bind f x
    let (>>) x y = bind (CCFun.const y) x
  end
end

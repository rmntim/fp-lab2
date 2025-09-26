(** AVL-based immutable dictionary. *)

module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type key
  type +'a t

  val empty : 'a t
  val is_empty : 'a t -> bool
  val singleton : key -> 'a -> 'a t
  val insert : key -> 'a -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val mem : key -> 'a t -> bool
  val find_opt : key -> 'a t -> 'a option
  val cardinal : 'a t -> int
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val fold_left : ('b -> key -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_right : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val append : 'a t -> 'a t -> 'a t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val to_list : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
  val minimum : 'a t -> (key * 'a) option
  val maximum : 'a t -> (key * 'a) option
end

module Make (Ord : OrderedType) : S with type key = Ord.t

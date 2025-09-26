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

module Make (Ord : OrderedType) : S with type key = Ord.t = struct
  type key = Ord.t

  type +'a tree =
    | Empty
    | Node of {
        key : key;
        value : 'a;
        height : int;
        left : 'a tree;
        right : 'a tree;
      }

  type +'a t = 'a tree

  let empty = Empty
  let is_empty = function Empty -> true | Node _ -> false
  let height = function Empty -> 0 | Node { height; _ } -> height

  let create left key value right =
    let height = 1 + Stdlib.max (height left) (height right) in
    Node { key; value; height; left; right }

  let balance_factor = function
    | Empty -> 0
    | Node { left; right; _ } -> height left - height right

  let rotate_left = function
    | Node
        {
          key = kx;
          value = vx;
          left = ax;
          right = Node { key = ky; value = vy; left = by; right = cy; _ };
          _;
        } ->
        create (create ax kx vx by) ky vy cy
    | tree -> tree

  let rotate_right = function
    | Node
        {
          key = ky;
          value = vy;
          right = cy;
          left = Node { key = kx; value = vx; left = ax; right = bx; _ };
          _;
        } ->
        create ax kx vx (create bx ky vy cy)
    | tree -> tree

  let balance = function
    | Empty -> Empty
    | Node { left; right; key; value; _ } ->
        let bf = height left - height right in
        if bf > 1 then
          let left' =
            match left with
            | Empty -> left
            | Node _ as l -> if balance_factor l < 0 then rotate_left l else l
          in
          rotate_right (create left' key value right)
        else if bf < -1 then
          let right' =
            match right with
            | Empty -> right
            | Node _ as r -> if balance_factor r > 0 then rotate_right r else r
          in
          rotate_left (create left key value right')
        else create left key value right

  let singleton key value = create Empty key value Empty

  let rec insert key value = function
    | Empty -> singleton key value
    | Node ({ key = k; value = v; left; right; _ } as _node) ->
        let cmp = Ord.compare key k in
        if cmp = 0 then create left key value right
        else if cmp < 0 then balance (create (insert key value left) k v right)
        else balance (create left k v (insert key value right))

  let find_opt key tree =
    let rec aux = function
      | Empty -> None
      | Node { key = k; value; left; right; _ } ->
          let cmp = Ord.compare key k in
          if cmp = 0 then Some value
          else if cmp < 0 then aux left
          else aux right
    in
    aux tree

  let mem key tree = Option.is_some (find_opt key tree)

  let rec remove_min = function
    | Empty -> invalid_arg "remove_min"
    | Node { key; value; left = Empty; right; _ } -> (right, key, value)
    | Node { key; value; left; right; _ } ->
        let left', min_key, min_value = remove_min left in
        (balance (create left' key value right), min_key, min_value)

  let rec remove key tree =
    match tree with
    | Empty -> Empty
    | Node { key = k; value = v; left; right; _ } ->
        let cmp = Ord.compare key k in
        if cmp = 0 then
          match (left, right) with
          | Empty, _ -> right
          | _, Empty -> left
          | _ ->
              let right', min_key, min_value = remove_min right in
              balance (create left min_key min_value right')
        else if cmp < 0 then balance (create (remove key left) k v right)
        else balance (create left k v (remove key right))

  let update key f tree =
    match f (find_opt key tree) with
    | None -> remove key tree
    | Some value -> insert key value tree

  let rec cardinal = function
    | Empty -> 0
    | Node { left; right; _ } -> 1 + cardinal left + cardinal right

  let rec minimum_opt = function
    | Empty -> None
    | Node { key; value; left = Empty; _ } -> Some (key, value)
    | Node { left; _ } -> minimum_opt left

  let rec maximum_opt = function
    | Empty -> None
    | Node { key; value; right = Empty; _ } -> Some (key, value)
    | Node { right; _ } -> maximum_opt right

  let rec fold_left f acc = function
    | Empty -> acc
    | Node { left; key; value; right; _ } ->
        let acc = fold_left f acc left in
        let acc = f acc key value in
        fold_left f acc right

  let rec fold_right f tree acc =
    match tree with
    | Empty -> acc
    | Node { left; key; value; right; _ } ->
        fold_right f left (f key value (fold_right f right acc))

  let map fn tree =
    fold_left (fun acc key value -> insert key (fn value) acc) empty tree

  let mapi fn tree =
    fold_left (fun acc key value -> insert key (fn key value) acc) empty tree

  let filter predicate tree =
    fold_left
      (fun acc key value ->
        if predicate key value then insert key value acc else acc)
      empty tree

  let append a b = fold_left (fun acc key value -> insert key value acc) b a

  let to_list tree =
    fold_right (fun key value acc -> (key, value) :: acc) tree []

  let of_list entries =
    List.fold_left (fun acc (key, value) -> insert key value acc) empty entries

  let minimum tree = minimum_opt tree
  let maximum tree = maximum_opt tree

  let rec push_left stack = function
    | Empty -> stack
    | Node { left; _ } as node -> push_left (node :: stack) left

  let rec next = function
    | [] -> None
    | Empty :: rest -> next rest
    | Node { key; value; right; _ } :: rest ->
        let stack' = push_left rest right in
        Some (key, value, stack')

  let equal value_equal a b =
    let rec loop stack_a stack_b =
      match (next stack_a, next stack_b) with
      | None, None -> true
      | Some _, None | None, Some _ -> false
      | Some (ka, va, stack_a'), Some (kb, vb, stack_b') ->
          Ord.compare ka kb = 0 && value_equal va vb && loop stack_a' stack_b'
    in
    loop (push_left [] a) (push_left [] b)
end

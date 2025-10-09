module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type key
  type empty
  type nonempty
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
  type empty
  type nonempty

  type (+'a, _) tree =
    | Empty : ('a, empty) tree
    | Node : {
        key : key;
        value : 'a;
        height : int;
        left : ('a, 'l) tree;
        right : ('a, 'r) tree;
      }
        -> ('a, nonempty) tree

  type +'a t = Ex : ('a, _) tree -> 'a t

  let empty = Ex Empty
  let is_empty (Ex t) = match t with Empty -> true | Node _ -> false

  let height : type a e. (a, e) tree -> int = function
    | Empty -> 0
    | Node { height; _ } -> height

  let create : type a l r.
      (a, l) tree -> key -> a -> (a, r) tree -> (a, nonempty) tree =
   fun left key value right ->
    let height = 1 + Stdlib.max (height left) (height right) in
    Node { key; value; height; left; right }

  let balance_factor : type a e. (a, e) tree -> int = function
    | Empty -> 0
    | Node { left; right; _ } -> height left - height right

  let rotate_left : type a e. (a, e) tree -> (a, e) tree = function
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

  let rotate_right : type a e. (a, e) tree -> (a, e) tree = function
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

  let balance : type a e. (a, e) tree -> (a, e) tree = function
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

  let singleton key value = Ex (create Empty key value Empty)

  let rec insert_aux : type a e. key -> a -> (a, e) tree -> (a, nonempty) tree =
   fun key value -> function
    | Empty -> create Empty key value Empty
    | Node { key = k; value = v; left; right; _ } ->
        let cmp = Ord.compare key k in
        if cmp = 0 then create left key value right
        else if cmp < 0 then
          balance (create (insert_aux key value left) k v right)
        else balance (create left k v (insert_aux key value right))

  let insert key value (Ex tree) = Ex (insert_aux key value tree)

  let rec find_opt_aux : type a e. key -> (a, e) tree -> a option =
   fun key -> function
    | Empty -> None
    | Node { key = k; value; left; right; _ } ->
        let cmp = Ord.compare key k in
        if cmp = 0 then Some value
        else if cmp < 0 then find_opt_aux key left
        else find_opt_aux key right

  let find_opt key (Ex tree) = find_opt_aux key tree
  let mem key tree = Option.is_some (find_opt key tree)

  type ('a, 'e) remove_min_result =
    | MinEmpty : ('a, empty) remove_min_result
    | MinNode : ('a, 'e) tree * key * 'a -> ('a, nonempty) remove_min_result

  let rec remove_min : type a e. (a, e) tree -> (a, e) remove_min_result =
    function
    | Empty -> MinEmpty
    | Node { key; value; left = Empty; right; _ } -> MinNode (right, key, value)
    | Node { key; value; left; right; _ } -> (
        match remove_min left with
        | MinEmpty -> MinNode (right, key, value)
        | MinNode (left', min_key, min_value) ->
            MinNode (balance (create left' key value right), min_key, min_value)
        )

  type ('a, 'e) remove_result =
    | RemoveEx : ('a, _) tree -> ('a, 'e) remove_result

  let rec remove_aux : type a e. key -> (a, e) tree -> (a, e) remove_result =
   fun key -> function
    | Empty -> RemoveEx Empty
    | Node { key = k; value = v; left; right; _ } ->
        let cmp = Ord.compare key k in
        if cmp = 0 then
          match (left, right) with
          | Empty, r -> RemoveEx r
          | l, Empty -> RemoveEx l
          | _ -> (
              match remove_min right with
              | MinEmpty -> RemoveEx left
              | MinNode (right', min_key, min_value) ->
                  RemoveEx (balance (create left min_key min_value right')))
        else if cmp < 0 then
          let (RemoveEx left') = remove_aux key left in
          RemoveEx (balance (create left' k v right))
        else
          let (RemoveEx right') = remove_aux key right in
          RemoveEx (balance (create left k v right'))

  let remove key (Ex tree) =
    let (RemoveEx tree') = remove_aux key tree in
    Ex tree'

  let update key f (Ex tree) =
    match f (find_opt_aux key tree) with
    | None ->
        let (RemoveEx tree') = remove_aux key tree in
        Ex tree'
    | Some value -> Ex (insert_aux key value tree)

  let rec cardinal_aux : type a e. (a, e) tree -> int = function
    | Empty -> 0
    | Node { left; right; _ } -> 1 + cardinal_aux left + cardinal_aux right

  let cardinal (Ex tree) = cardinal_aux tree

  let rec minimum_opt_aux : type a e. (a, e) tree -> (key * a) option = function
    | Empty -> None
    | Node { key; value; left = Empty; _ } -> Some (key, value)
    | Node { left; _ } -> minimum_opt_aux left

  let minimum (Ex tree) = minimum_opt_aux tree

  let rec maximum_opt_aux : type a e. (a, e) tree -> (key * a) option = function
    | Empty -> None
    | Node { key; value; right = Empty; _ } -> Some (key, value)
    | Node { right; _ } -> maximum_opt_aux right

  let maximum (Ex tree) = maximum_opt_aux tree

  let rec fold_left_aux : type a e b.
      (b -> key -> a -> b) -> b -> (a, e) tree -> b =
   fun f acc -> function
    | Empty -> acc
    | Node { left; key; value; right; _ } ->
        let acc = fold_left_aux f acc left in
        let acc = f acc key value in
        fold_left_aux f acc right

  let fold_left f acc (Ex tree) = fold_left_aux f acc tree

  let rec fold_right_aux : type a e b.
      (key -> a -> b -> b) -> (a, e) tree -> b -> b =
   fun f tree acc ->
    match tree with
    | Empty -> acc
    | Node { left; key; value; right; _ } ->
        fold_right_aux f left (f key value (fold_right_aux f right acc))

  let fold_right f (Ex tree) acc = fold_right_aux f tree acc

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

  type 'a stack_item = StackEx : ('a, _) tree -> 'a stack_item

  let rec push_left : type a e.
      a stack_item list -> (a, e) tree -> a stack_item list =
   fun stack tree ->
    match tree with
    | Empty -> stack
    | Node { left; _ } -> push_left (StackEx tree :: stack) left

  let rec next : type a.
      a stack_item list -> (key * a * a stack_item list) option = function
    | [] -> None
    | StackEx Empty :: rest -> next rest
    | StackEx (Node { key; value; right; _ }) :: rest ->
        let stack' = push_left rest right in
        Some (key, value, stack')

  let equal value_equal (Ex a) (Ex b) =
    let rec loop stack_a stack_b =
      match (next stack_a, next stack_b) with
      | None, None -> true
      | Some _, None | None, Some _ -> false
      | Some (ka, va, stack_a'), Some (kb, vb, stack_b') ->
          Ord.compare ka kb = 0 && value_equal va vb && loop stack_a' stack_b'
    in
    loop (push_left [] a) (push_left [] b)
end

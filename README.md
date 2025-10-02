# Labwork 2 — Иммутабельный словарь (AVL)

Функционально-полиморфная библиотека ассоциативного массива (словаря),
реализованная поверх самобалансирующегося AVL-дерева. Структура данных
неизменяема, поддерживает основные операции коллекций, свёртки, `map`/`filter`,
моноидальную операцию объединения, а также эффективное сравнение на уровне API.

## Структура репозитория

- `lib/dict.mli`, `lib/dict.ml` — интерфейс и реализация словаря (AVL)
- `lib/labwork2.mli`, `lib/labwork2.ml` — публичный модуль, реэкспорт `Dict`
- `test/test_labwork2.ml` — unit и property-based тесты (Alcotest + QCheck)
- `bin/main.ml` — небольшой исполняемый пример
- `dune-project`, `lib/dune`, `test/dune`, `bin/dune` — сборка Dune
- `.github/workflows` — CI: сборка, тесты, проверка стиля (`zanuda`)

## Требования к разработанному ПО

- Функциональные (по заданию):
  - добавление/удаление элементов (`insert`/`remove`/`update`);
  - фильтрация (`filter`);
  - отображение (`map`, `mapi`);
  - свёртки (левая/правая: `fold_left`, `fold_right`);
  - структура образует моноид (`empty` — нейтральный элемент, `append` — операция);
  - полиморфизм по значению, параметризуемость по типу ключа (через `OrderedType`).
- Нефункциональные/технические:
  - неизменяемость структуры данных;
  - разделение интерфейса/реализации (`.mli`/`.ml`), тесты поверх публичного API;
  - эффективное сравнение словарей без наивной конверсии к спискам;
  - идиоматичный стиль OCaml; совместимость с Dune/Opam, CI.
- Окружение/зависимости:
  - OCaml (в CI используется компилятор ветки 4.x);
  - Dune ≥ 3.20;
  - Тесты: `alcotest`, `qcheck`, `qcheck-alcotest`;
  - Dev: `zanuda` (проверка стиля/настроек).

## Сборка и запуск

```sh
opam install . --deps-only --with-test --with-dev-setup
opam exec -- dune build
opam exec -- dune runtest     # запускает unit и property-based тесты
# опционально
opam exec -- dune exec labwork2
```

## Ключевые элементы реализации

Ниже — выдержки кода с минимальными комментариями; см. полные тексты в `lib/`.

- Интерфейс параметризуемого словаря (ключи с упорядочиванием): `lib/dict.mli`

```ocaml
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
```

- Внутренняя структура — AVL-дерево: `lib/dict.ml`

```ocaml
type +'a tree =
  | Empty
  | Node of { key : key; value : 'a; height : int; left : 'a tree; right : 'a tree }
```

- Балансировки AVL (левый/правый поворот + баланс):

```ocaml
let rotate_left = function
  | Node { key = kx; value = vx; left = ax; right = Node { key = ky; value = vy; left = by; right = cy; _ }; _ } ->
      create (create ax kx vx by) ky vy cy
  | tree -> tree

let rotate_right = function
  | Node { key = ky; value = vy; right = cy; left = Node { key = kx; value = vx; left = ax; right = bx; _ }; _ } ->
      create ax kx vx (create bx ky vy cy)
  | tree -> tree

let balance = function
  | Empty -> Empty
  | Node { left; right; key; value; _ } ->
      let bf = height left - height right in
      if bf > 1 then
        let left' = match left with Empty -> left | Node _ as l -> if balance_factor l < 0 then rotate_left l else l in
        rotate_right (create left' key value right)
      else if bf < -1 then
        let right' = match right with Empty -> right | Node _ as r -> if balance_factor r > 0 then rotate_right r else r in
        rotate_left (create left key value right')
      else create left key value right
```

- Базовые операции вставки/поиска/удаления:

```ocaml
let rec insert key value = function
  | Empty -> singleton key value
  | Node { key = k; value = v; left; right; _ } ->
      let cmp = Ord.compare key k in
      if cmp = 0 then create left key value right
      else if cmp < 0 then balance (create (insert key value left) k v right)
      else balance (create left k v (insert key value right))

let rec remove key = function
  | Empty -> Empty
  | Node { key = k; value = v; left; right; _ } ->
      let cmp = Ord.compare key k in
      if cmp = 0 then (* удаление узла *) ...
      else if cmp < 0 then balance (create (remove key left) k v right)
      else balance (create left k v (remove key right))

let update key f tree =
  match f (find_opt key tree) with
  | None -> remove key tree
  | Some value -> insert key value tree
```

- Свёртки и производные операции (`map`, `mapi`, `filter`):

```ocaml
let rec fold_left f acc = function
  | Empty -> acc
  | Node { left; key; value; right; _ } ->
      let acc = fold_left f acc left in
      let acc = f acc key value in
      fold_left f acc right

let map fn tree =
  fold_left (fun acc key value -> insert key (fn value) acc) empty tree

let filter p tree =
  fold_left (fun acc k v -> if p k v then insert k v acc else acc) empty tree
```

- Моноид (нейтральный элемент и ассоциативная операция объединения):

```ocaml
let empty = Empty
let append a b = fold_left (fun acc k v -> insert k v acc) b a
```

- Эффективное сравнение API-уровня (ин-order обход двух деревьев без явной
  материализации обоих списков):

```ocaml
let equal value_equal a b =
  let rec loop stack_a stack_b =
    match (next stack_a, next stack_b) with
    | None, None -> true
    | Some _, None | None, Some _ -> false
    | Some (ka, va, sa'), Some (kb, vb, sb') ->
        Ord.compare ka kb = 0 && value_equal va vb && loop sa' sb'
  in
  loop (push_left [] a) (push_left [] b)
```

## Полиморфизм и неизменяемость

- Тип `'a t` полиморфен по значению; тип ключа задаётся модулем `OrderedType`.
- Все операции создают новые версии дерева, не изменяя существующее состояние.
- Публичный интерфейс отделён от реализации (`dict.mli`/`dict.ml`),
  модуль `Labwork2` реэкспортирует `Dict`.

## Пример использования

```ocaml
open Labwork2
module IntOrd = struct type t = int let compare = Stdlib.compare end
module IntDict = Dict.Make(IntOrd)

let d = IntDict.empty |> IntDict.insert 1 "a" |> IntDict.insert 2 "b"
let d' = IntDict.map String.uppercase_ascii d
let only_odd = IntDict.filter (fun k _ -> k mod 2 = 1) d'
```

## Тестирование

- Unit-тесты: вставка/поиск, удаление, `map`/`filter`, свёртки.
- Property-based: как минимум 3 свойствa (включая моноид):
  - левый нейтральный: `append empty x == x`;
  - правый нейтральный: `append x empty == x`;
  - ассоциативность: `append (append x y) z == append x (append y z)`;
  - дополнительное: композиция `map`.

Запуск локально: `opam exec -- dune runtest`.

### Отчёт о запуске тестов

```sh
> dune runtest -f
qcheck random seed: 655274660
Testing `Labwork2 Dict'.
This run has ID `YGQK0ZPX'.

  [OK]          unit                0   insert & find.
  [OK]          unit                1   remove.
  [OK]          unit                2   map & filter.
  [OK]          unit                3   folds.
  [OK]          properties          0   append left identity.
  [OK]          properties          1   append right identity.
  [OK]          properties          2   append associativity.
  [OK]          properties          3   map composition.

Full test results in `~/Programming/ITMO/Semester5/FP/Labwork2/_build/default/test/_build/_tests/Labwork2 Dict'.
Test Successful in 0.021s. 8 tests run.
```

### Покрытие кода

https://rmntim.github.io/fp-lab2/

## Выводы

- Использование параметрических модулей (функторов) позволяет отделить
  семантику (упорядочивание ключей) от структуры данных и сохранить
  полиморфизм.
- AVL-дерево обеспечивает гарантированное `O(log n)` для `insert/remove/find`,
  а операции высшего порядка (`map/filter/fold`) линейны по размеру.
- Эффективное сравнение реализовано без наивной конверсии к спискам, через
  синхронный inorder-обход, что сохраняет асимптотику и снижает давление на GC.
- Разделение интерфейса/реализации и тестирование на уровне API делает код
  устойчивым к изменению внутреннего представления.
- Property-based тестирование удобно для проверки алгебраических законов (моноид)
  и взаимодействия операций (`map`), дополняя традиционные unit-тесты.

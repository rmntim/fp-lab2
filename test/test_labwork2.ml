open Labwork2

module IntOrd = struct
  type t = int

  let compare = Stdlib.compare
end

module IntDict = Dict.Make (IntOrd)

let dict_testable =
  let open Format in
  let pp fmt dict =
    let bindings = IntDict.to_list dict in
    let pp_binding fmt (k, v) = fprintf fmt "%d:%d" k v in
    fprintf fmt "[%a]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp_binding)
      bindings
  in
  Alcotest.testable pp (IntDict.equal ( = ))

let test_insert_and_find () =
  let open Alcotest in
  let dict =
    IntDict.empty |> IntDict.insert 3 30 |> IntDict.insert 1 10
    |> IntDict.insert 2 20
  in
  check bool "contains inserted key" true (IntDict.mem 3 dict);
  check (option int) "find existing key" (Some 10) (IntDict.find_opt 1 dict);
  check (option int) "find another key" (Some 20) (IntDict.find_opt 2 dict);
  check (option int) "missing key" None (IntDict.find_opt 42 dict)

let test_remove () =
  let open Alcotest in
  let dict =
    IntDict.empty |> IntDict.insert 5 50 |> IntDict.insert 3 30
    |> IntDict.insert 7 70 |> IntDict.insert 6 60
  in
  let dict = IntDict.remove 3 dict in
  check bool "removed key" false (IntDict.mem 3 dict);
  check (option int) "unrelated key" (Some 50) (IntDict.find_opt 5 dict);
  let dict = IntDict.remove 5 dict in
  check bool "second removal" false (IntDict.mem 5 dict);
  check dict_testable "expected remaining bindings"
    (IntDict.of_list [ (6, 60); (7, 70) ])
    dict;
  check
    (option (pair int int))
    "minimum updates"
    (Some (6, 60))
    (IntDict.minimum dict);
  check
    (option (pair int int))
    "maximum updates"
    (Some (7, 70))
    (IntDict.maximum dict)

let test_map_and_filter () =
  let open Alcotest in
  let dict =
    IntDict.empty |> IntDict.insert 1 1 |> IntDict.insert 2 2
    |> IntDict.insert 3 3
  in
  let doubled = IntDict.map (fun v -> v * 2) dict in
  check (option int) "map doubles value" (Some 4) (IntDict.find_opt 2 doubled);
  let filtered = IntDict.filter (fun key _ -> key mod 2 = 1) doubled in
  check bool "keeps odd key" true (IntDict.mem 1 filtered);
  check bool "filters even key" false (IntDict.mem 2 filtered);
  check int "cardinal after filter" 2 (IntDict.cardinal filtered);
  check dict_testable "filtered dictionary"
    (IntDict.of_list [ (1, 2); (3, 6) ])
    filtered

let test_fold_functions () =
  let open Alcotest in
  let dict = IntDict.of_list [ (1, 10); (2, 20); (3, 30) ] in
  let sum_left =
    IntDict.fold_left (fun acc key value -> acc + (key * 10) + value) 0 dict
  in
  let sum_right =
    IntDict.fold_right (fun key value acc -> acc + (key * 10) + value) dict 0
  in
  check int "fold_left sum" 120 sum_left;
  check int "folds consistent" sum_left sum_right

let bindings_arbitrary =
  let open QCheck in
  small_list (pair (int_range 0 1000) small_int)

let dict_of_bindings bindings = IntDict.of_list bindings

let property_append_left_identity =
  let open QCheck in
  Test.make ~name:"append left identity" ~count:200 bindings_arbitrary
    (fun bs ->
      let dict = dict_of_bindings bs in
      IntDict.equal ( = ) dict (IntDict.append IntDict.empty dict))

let property_append_right_identity =
  let open QCheck in
  Test.make ~name:"append right identity" ~count:200 bindings_arbitrary
    (fun bs ->
      let dict = dict_of_bindings bs in
      IntDict.equal ( = ) dict (IntDict.append dict IntDict.empty))

let property_append_associative =
  let open QCheck in
  Test.make ~name:"append associativity" ~count:200
    (triple bindings_arbitrary bindings_arbitrary bindings_arbitrary)
    (fun (a, b, c) ->
      let da = dict_of_bindings a in
      let db = dict_of_bindings b in
      let dc = dict_of_bindings c in
      let left = IntDict.append (IntDict.append da db) dc in
      let right = IntDict.append da (IntDict.append db dc) in
      IntDict.equal ( = ) left right)

let property_map_composition =
  let open QCheck in
  let f x = x + 1 in
  let g x = x * 2 in
  Test.make ~name:"map composition" ~count:200 bindings_arbitrary (fun bs ->
      let dict = dict_of_bindings bs in
      let mapped = IntDict.map f (IntDict.map g dict) in
      let composed = IntDict.map (fun x -> f (g x)) dict in
      IntDict.equal ( = ) composed mapped)

let property_tests =
  [
    property_append_left_identity;
    property_append_right_identity;
    property_append_associative;
    property_map_composition;
  ]
  |> List.map QCheck_alcotest.to_alcotest

let unit_tests =
  let open Alcotest in
  [
    test_case "insert & find" `Quick test_insert_and_find;
    test_case "remove" `Quick test_remove;
    test_case "map & filter" `Quick test_map_and_filter;
    test_case "folds" `Quick test_fold_functions;
  ]

let () =
  Alcotest.run "Labwork2 Dict"
    [ ("unit", unit_tests); ("properties", property_tests) ]

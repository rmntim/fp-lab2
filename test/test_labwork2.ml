open Labwork2

module IntOrd = struct
  type t = int

  let compare = Stdlib.compare
end

module IntDict = Dict.Make (IntOrd)

let expect ?(msg = "unexpected failure") condition =
  if not condition then failwith msg

let expect_dict_equal ~expected ~actual =
  if not (IntDict.equal ( = ) expected actual) then
    let to_string dict =
      dict |> IntDict.to_list
      |> List.map (fun (k, v) -> Printf.sprintf "%d:%d" k v)
      |> String.concat ", "
    in
    failwith
      (Printf.sprintf "dictionaries differ\nexpected: [%s]\nactual:   [%s]"
         (to_string expected) (to_string actual))

let test_insert_and_find () =
  let dict =
    IntDict.empty |> IntDict.insert 3 30 |> IntDict.insert 1 10
    |> IntDict.insert 2 20
  in
  expect (IntDict.mem 3 dict);
  expect (IntDict.find_opt 1 dict = Some 10);
  expect (IntDict.find_opt 2 dict = Some 20);
  expect (IntDict.find_opt 42 dict = None);
  ()

let test_remove () =
  let dict =
    IntDict.empty |> IntDict.insert 5 50 |> IntDict.insert 3 30
    |> IntDict.insert 7 70 |> IntDict.insert 6 60
  in
  let dict = IntDict.remove 3 dict in
  expect (not (IntDict.mem 3 dict));
  expect (IntDict.find_opt 5 dict = Some 50);
  let dict = IntDict.remove 5 dict in
  expect (not (IntDict.mem 5 dict));
  expect (IntDict.minimum dict = Some (6, 60));
  expect (IntDict.maximum dict = Some (7, 70))

let test_map_and_filter () =
  let dict =
    IntDict.empty |> IntDict.insert 1 1 |> IntDict.insert 2 2
    |> IntDict.insert 3 3
  in
  let doubled = IntDict.map (fun v -> v * 2) dict in
  expect (IntDict.find_opt 2 doubled = Some 4);
  let filtered = IntDict.filter (fun key _ -> key mod 2 = 1) doubled in
  expect (IntDict.mem 1 filtered);
  expect (not (IntDict.mem 2 filtered));
  expect (IntDict.cardinal filtered = 2)

let test_fold_functions () =
  let dict = IntDict.of_list [ (1, 10); (2, 20); (3, 30) ] in
  let sum_left =
    IntDict.fold_left (fun acc key value -> acc + (key * 10) + value) 0 dict
  in
  let sum_right =
    IntDict.fold_right (fun key value acc -> acc + (key * 10) + value) dict 0
  in
  expect (sum_left = sum_right);
  expect (sum_left = 120)

(* -------------------------------------------------------------------------- *)
(* Property-based tests                                                       *)

let random_dict ?(max_key = 1000) size =
  let rec build n acc =
    if n = 0 then acc
    else
      let key = Random.int max_key in
      let value = Random.int 10000 in
      build (n - 1) ((key, value) :: acc)
  in
  IntDict.of_list (build size [])

let property_append_left_identity iterations =
  for _ = 1 to iterations do
    let d = random_dict 25 in
    expect_dict_equal ~expected:d ~actual:(IntDict.append IntDict.empty d)
  done

let property_append_right_identity iterations =
  for _ = 1 to iterations do
    let d = random_dict 25 in
    expect_dict_equal ~expected:d ~actual:(IntDict.append d IntDict.empty)
  done

let property_append_associative iterations =
  for _ = 1 to iterations do
    let a = random_dict 20 in
    let b = random_dict 20 in
    let c = random_dict 20 in
    let left = IntDict.append (IntDict.append a b) c in
    let right = IntDict.append a (IntDict.append b c) in
    expect_dict_equal ~expected:left ~actual:right
  done

let property_map_composition iterations =
  let f x = x + 1 in
  let g x = x * 2 in
  for _ = 1 to iterations do
    let d = random_dict 20 in
    let mapped = IntDict.map f (IntDict.map g d) in
    let composed = IntDict.map (fun x -> f (g x)) d in
    expect_dict_equal ~expected:composed ~actual:mapped
  done

let () =
  Random.init 0xC0FFEE;
  let tests =
    [
      ("insert & find", test_insert_and_find);
      ("remove", test_remove);
      ("map & filter", test_map_and_filter);
      ("folds", test_fold_functions);
    ]
  in
  List.iter
    (fun (name, fn) ->
      try fn ()
      with exn ->
        Printf.eprintf "Unit test '%s' failed: %s\n" name
          (Printexc.to_string exn);
        exit 1)
    tests;
  let properties =
    [
      ("append left identity", property_append_left_identity);
      ("append right identity", property_append_right_identity);
      ("append associativity", property_append_associative);
      ("map composition", property_map_composition);
    ]
  in
  List.iter
    (fun (name, property) ->
      try property 200
      with exn ->
        Printf.eprintf "Property '%s' failed: %s\n" name
          (Printexc.to_string exn);
        exit 1)
    properties;
  print_endline "All tests passed"

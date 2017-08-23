open OUnit2

module W = Typing.Typecheck.W
(* open W.Infix *)

exception ParseError


let test_parse_pp_str ?(isTodo=false) input expected_output _ =
  if isTodo then todo "Not implemented yet";
  let output =
    begin
      match Parse.Parser.parse_string Parse.Parser.expr input with
      | Ok x ->
        let simple = Simple.Of_onix.expr x in
        if W.log simple = [] then
          Simple.Pp.pp_expr Format.str_formatter (W.value simple)
        else
          ()
      | Error _ -> raise ParseError
    end;
    Format.flush_str_formatter ()
  in
  assert_equal
    ~printer:(fun x -> x)
    expected_output
    output

let test_parse_pp_str_fail ?(isTodo=false) input _ =
  if isTodo then todo "Not implemented yet";
  begin
    match Parse.Parser.parse_string Parse.Parser.expr input with
    | Ok x ->
      begin
        try
          let s = Simple.Of_onix.expr x in
          if W.log s = [] then
            assert_failure "Translation error not raised"
          else ()
        with
          Failure _ -> ()
      end

    | Error _ -> raise ParseError
  end

let isTodo = true (* To use [~isTodo] as a shortcut for [~isTodo=true] *)

let testsuite =
  "nix_light">:::
  List.map (fun (name, input, output) ->
      name >:: test_parse_pp_str input output)
    [
      "test_var", "x", "x";
      "test_const_int", "1234", "1234";
      "test_const_true", "true", "true";
      "test_const_false", "false", "false";
      "test_lambda", "x: x", "(x: x)";
      "test_app", "x y", "x y";
      "test_lambda_app", "(x: y) z", "(x: y) z";
      "test_app_lambda", "x: y z", "(x: y z)";
      "test_Y_comb", "(x: x x) (x: x x)", "(x: x x) (x: x x)";
      "test_annot", "(x /*: int */)", "(x /*: int */)";
      "test_annot_arrow", "(x /*: int -> int */)", "(x /*: (int) -> int */)";
      "test_string", "\"x\"", "\"x\"";
      "test_list", "[1 2 3]", "Cons(1, Cons(2, Cons(3, nil)))";
      "test_record_1", "{ x = 1; y = 2; }", "{ \"x\" = 1; \"y\" = 2; }";
      "test_record_2", "{ x.y = 1; }", "{ \"x\" = { \"y\" = 1; }; }";
      ("test_record_3",
       "{ x.y = 1; x.z = 2; }",
       "{ \"x\" = { \"y\" = 1; \"z\" = 2; }; }");
      "test_apath", "x.y.${\"z\"}", "x.\"y\".\"z\"";
      "test_pattern_record", "{}:x", "({  }: x)";
      "test_pattern_record_alias", "{}@x:x", "({  }@x: x)";
      ("test_pattern_record_default", "{ x ? 1 }: x",
       "({ x? }: let %%x = x; in let x = if (%%isUndef %%x) \
        then 1 else %%x; in x)");
      ("test_recursive_record",
       "rec { x = 1; y = x; }",
       "let x = 1; y = x; in { \"x\" = x; \"y\" = y; }");
      "test_path", "./foo", "./foo";
      "test_with", "with e1; e2", "with e1; e2";
      "test_apath_or", "x.y or z", "x.\"y\" or z";
      "test_bracket", "<foo>", "<foo>";
    ] @
  List.map (fun (name, input) ->
      name >:: test_parse_pp_str_fail input)
    [
      "test_record_fail_1", "{ x = 1; x = 2; }";
      "test_record_fail_2", "{ x.y = 1; x.y = 2; }";
      "test_record_pattern_fail", "{ x, x }: x";
    ]

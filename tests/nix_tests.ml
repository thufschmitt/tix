open OUnit2

let () = run_test_tt_main
    ("all_tests">::: [
        Tests_onix.testsuite;
        Tests_nix_light.testsuite;
        Test_typecheck.testsuite;
      ])

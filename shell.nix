{
  nixpkgs ? <nixpkgs>, system ? builtins.currentSystem
}:
with import nixpkgs { inherit system; };
let ocaml_wrapped =
  pkgs.symlinkJoin {
    name = "ocaml-wrapped";
    paths = [ pkgs.ocamlPackages.ocaml ];
    buildInputs = [ makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/ocaml \
      --add-flags "-I ${ocamlPackages.findlib}/lib/ocaml/${lib.getVersion ocamlPackages.ocaml}/site-lib"
    '';
  };
  opal = ocamlPackages.buildOcaml {
    name = "opal";
    version = "0.1.1";

    src = fetchFromGitHub {
      owner = "pyrocat101";
      repo = "opal";
      rev = "v0.1.1";
      sha256 = "0qzsasjgbcjk66r9mc3q1ygq1l1g9sm967rhhcxgzlz1jqbaya7b";
      };

      installTargets = [ "libinstall" ];
    };
in
stdenv.mkDerivation rec {
  name = "onix";
  version = "0.0";
  propagatedBuildInputs = with ocamlPackages; [
    findlib
    ocamlbuild ocaml_oasis
    ounit
    ocaml_wrapped
    ppx_deriving
    opal
    containers
  ];

  src = ./.;
}



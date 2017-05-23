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
  jbuilder = ocamlPackages.buildOcaml rec {
    name = "jbuilder-${version}";
    version = "1.0+beta9";

    src = fetchFromGitHub {
      owner = "janestreet";
      repo = "jbuilder";
      rev = version;
      sha256 = "1pyvv3l3q5754lfwf2yjahrplg6ssnyn94q29gb38p1s0vq5pys7";
    };

    buildPhase = ''
      ocaml bootstrap.ml
      ./boot.exe -j 4
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp ./_build/default/bin/main.exe $out/bin/jbuilder
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
    opal
    containers
    jbuilder
  ];

  src = ./.;
}



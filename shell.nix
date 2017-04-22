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
in
stdenv.mkDerivation rec {
  name = "onix";
  version = "0.0";
  propagatedBuildInputs = with ocamlPackages; [
    findlib
    ocamlbuild ocaml_oasis
    ounit
    menhir
    ocaml_wrapped
    ppx_deriving
    containers
  ];

  src = ./.;
}



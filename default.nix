{
  nixpkgs ? <nixpkgs>, system ? builtins.currentSystem
}:
with import nixpkgs { inherit system; };
ocamlPackages.buildOcaml rec {
  name = "onix";
  version = "0.0";
  createFindlibDestdir = false;
  propagatedBuildInputs = with ocamlPackages; [
    ocaml ocamlbuild ocaml_oasis
    menhir
  ];

  src = ./.;
}



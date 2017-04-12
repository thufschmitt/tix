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
ocamlPackages.buildOcaml rec {
  name = "onix";
  version = "0.0";
  createFindlibDestdir = false;
  propagatedBuildInputs = with ocamlPackages; [
    ocamlbuild ocaml_oasis
    ounit
    menhir
    ocaml_wrapped
    ppx_deriving
  ];

  # Hack to make the wrapper the real ocaml
  shellHook = ''
    export PATH=${ocaml_wrapped}/bin:$PATH
  '';

  preConfigure = shellHook;

  src = ./.;
}



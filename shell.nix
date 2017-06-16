{
  nixpkgs' ? null, system ? builtins.currentSystem
}:
let
  nixpkgs = if nixpkgs' != null then nixpkgs' else
    let nixpkgs' = import <nixpkgs> {}; in
    nixpkgs'.fetchFromGitHub {
      # Waiting for the containers library to be updated upstream
      owner = "regnat";
      repo = "nixpkgs";
      rev = "d036d882965989f38a9a414a3f34b692d146bdf6";
      sha256 = "1njsk8jbwlzqgdazmxcvaxwmkxam61b8zjidsr6hawi1dmlflhfh";
    };
in
with import nixpkgs { inherit system; };
let
  ocamlPackages = ocamlPackages_4_03;
  ocaml = ocamlPackages.ocaml;
  cduce-lib = stdenv.mkDerivation rec {
    name = "cduce-unstable-${version}";
    version = "2016-06-07";

    src = fetchgit {
      url = "https://gitlab.math.univ-paris-diderot.fr/cduce/cduce.git/";
      rev = "6d44f428306e588c8f0177a43aa7794810f02d60";
      sha256 = "1czij3ndqr75ifj0sf51y8w6za6clq5plv2m8x4m15mfk8l6ir84";
    };

    propagatedBuildInputs = with ocamlPackages; [
      ocaml
      findlib
      ocaml_pcre ulex
      rlwrap
    ];

    createFindlibDestdir = true;

    preConfigure = ''
      sed -i 's@+camlp4/camlp4lib.cma @@' META.in
      sed -i 's@+camlp4/camlp4lib.cmxa @@' META.in
      sed -i 's/requires="/requires="camlp4.lib /' META.in

      # Also install .cmx file to prevent a compiler warning
      sed -i 's#lib/cduce_lib.a#lib/cduce_lib.a lib/cduce_lib.cmx#' \
        Makefile.distrib
    '';

    configureFlags = [
      "--without-pxp"
      "--without-expat"
      "--without-curl"
      "--without-netclient"
      "--without-cgi"
    ];

    installTargets = [ "install_lib" ];

  };
in
stdenv.mkDerivation rec {
  name = "onix";
  version = "0.0";
  propagatedBuildInputs = with ocamlPackages; [
    ocaml
    findlib
    ounit
    containers
    jbuilder
    cduce-lib
    cmdliner_1_0
    mparser
  ];

  src = builtins.filterSource (name: type:
    let baseName = baseNameOf (toString name); in !(
    (type == "directory" && (baseName == ".git" ||
                             baseName == "_build" ||
                             baseName == "_obuild" ||
                             baseName == ".merlin" ||
                             lib.hasSuffix ".install" baseName))
    ))
  ./.;
}

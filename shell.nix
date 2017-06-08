{
  nixpkgs ? <nixpkgs>, system ? builtins.currentSystem
}:
with import nixpkgs { inherit system; };
let
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
  cduce-lib = stdenv.mkDerivation rec {
    name = "cduce-unstable-${version}";
    version = "2016-06-07";

    src = fetchgit {
      url = "https://gitlab.math.univ-paris-diderot.fr/cduce/cduce.git/";
      rev = "6d44f428306e588c8f0177a43aa7794810f02d60";
      sha256 = "1czij3ndqr75ifj0sf51y8w6za6clq5plv2m8x4m15mfk8l6ir84";
    };

    propagatedBuildInputs = with ocamlPackages_4_02; [
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
    opal
    containers
    jbuilder
    cduce-lib
    cmdliner
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

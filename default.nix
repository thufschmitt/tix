{
  nixpkgs ? <nixpkgs>, system ? builtins.currentSystem
}:
with import nixpkgs { inherit system; };
ocamlPackages.buildOcaml rec {
  name = "onix";
  version = "0.0";
  createFindlibDestdir = false;
  propagatedBuildInputs = [
    ocamlPackages.ocpBuild
  ] ++ pkgs.envs.camlEnv;

  src = ./.;
}



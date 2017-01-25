{
  nixpkgs ? <nixpkgs>, system ? builtins.currentSystem
}:
with import nixpkgs { inherit system; };
ocamlPackages.buildOcaml rec {
  name = "f_gadt";
  version = "0.0";
  shellHook = ''
  export SHELL=$(${busybox}/bin/which zsh)
  export REALRPROMPT=" — ${name}"
  exec zsh
  '';
  createFindlibDestdir = false;
  propagatedBuildInputs = [
    ocamlPackages.ocpBuild
  ] ++ pkgs.envs.camlEnv;

  src = ./.;
}



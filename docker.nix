{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.propellor;
let
  executable = propellor.propellor.components.exes.propellor;
  binOnly = prj.pkgs.runCommand "propellor-bin" { } ''
    mkdir -p $out/bin
    cp ${executable}/bin/propellor $out/bin
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/propellor
  '';
in { 
  propellor-image = prj.pkgs.dockerTools.buildImage {
  name = "propellor";
  tag = executable.version;
  contents = [ binOnly prj.pkgs.cacert prj.pkgs.iana-etc ];
  config.Entrypoint = "propellor";
  config.Cmd = "--help";
  };
}) crossBuildProject

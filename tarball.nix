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

  tarball = nativePkgs.stdenv.mkDerivation {
    name = "propellor-tarball";
    buildInputs = with nativePkgs; [ zip ];

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/
      zip -r -9 $out/propellor-tarball.zip ${binOnly}
    '';
  };
in {
 propellor-tarball = tarball;
}
) crossBuildProject

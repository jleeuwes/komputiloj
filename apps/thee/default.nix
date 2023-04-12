{ users, pkgs }:
with import ../../utilecoj.nix;
with builtins;
{
    activate = rec {
        pkg = pkgs.callPackage ./activation.nix { inherit users; };
        cmd = "${pkg}/bin/activate";
    };
}

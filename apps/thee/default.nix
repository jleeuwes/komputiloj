{ boltons, users, nixpkgs, ... }:
with boltons;
{
    activate = rec {
        pkg = nixpkgs.callPackage ./activation.nix { inherit boltons users; };
        cmd = "${pkg}/bin/activate";
    };
}

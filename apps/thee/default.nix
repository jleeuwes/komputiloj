{ boltons, capsules, ... }:
with boltons;
{
    activate = rec {
        pkg = capsules.nixpkgsCurrent.callPackage ./activation.nix {
            inherit boltons;
            users = capsules.all.users;
        };
        cmd = "${pkg}/bin/activate";
    };
}

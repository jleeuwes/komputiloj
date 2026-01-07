# Capsule for packages that are newer versions of packages from nixpkgs.
# Could be copied from some nixos unstable or just tweaked by us.
# In the latter case, if stuff works, we should contribute a PR
# to get the newer version into nixpkgs proper.
{ platform, nixos, nixos_25_05 }:
with builtins;
let
localSystem = platform.localSystem;
modernize = old: new: trace ("🕑🕐🕛 nixos_future: defining " + new.name + " to use instead of " + old.name) new;
callPackageWith = nixos.lib.callPackageWith;
self = {
    all = platform.lib.makeAll self;

    native.${localSystem} = let
        callPackage = callPackageWith nixos_25_05.native.${localSystem}.legacyPackages;
    in {
        packages = {
            # TODO FIXME compilating vengi-tools segfaults if we use NixOS 25.11
            vengi-tools = modernize
                nixos.native.${localSystem}.legacyPackages.vengi-tools
                (callPackage ./pkgs/vengi-tools.nix {
                    # for some reason not present in top-level packages when going through the flake:
                    libX11 = nixos_25_05.native.${localSystem}.legacyPackages.xorg.libX11;
                });
        };
    };
};
in self

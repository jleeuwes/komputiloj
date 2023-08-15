# TODO this file should become the anchor point for all komputiloj operations
let boltons = import ./lib/boltons.nix;
in with boltons;
let
    sources = importDir ./sources.d;
    default_nixos = "nixos_23_05"; # defines default nixos used by all of komputiloj
    default_nixos_source = getAttr default_nixos sources;
    komputiloj_capsule = {
        users = importDir ./users.d;
        packages = let
            callPackage = pkg: capsules.nixpkgsCurrent.callPackage pkg (capsules // { inherit boltons; });
        in rec {
            wachtwoord = callPackage ./pkgs/wachtwoord;
            radicale-commit-hook = callPackage ./pkgs/radicale-commit-hook;
            tipctl = callPackage ./pkgs/tipctl;
            dekstop = callPackage ./pkgs/dekstop;
            # git-annex-remote-rclone is not here but in nixpkgsFuture,
            # because we will remove our copy of the package when the
            # desired version is included in nixpkgs
        };
        modules = {
            dekstopomveging = (import ./modules/dekstopomveging) (capsules // { inherit boltons; });
        };

        overlays = rec {
            undesired-packages = import ./overlays/undesired-packages.nix { inherit boltons; };
        };
    };
    real_capsules = {
        komputiloj = komputiloj_capsule;
        hello-infra = sources.hello-infra.value {
            inherit boltons;
            inherit capsules;
        };
        gorinchemindialoog = sources.gorinchemindialoog.value;
        wolk = (import ./apps/wolk) (capsules // { inherit boltons; });
    };
    all_capsule = let
        cs = attrValues real_capsules;
    in {
        # special capsule which aggregates stuff from all other capsules
        users =
            # TODO merge with check for duplicates
            mergeAttrs (catAttrs "users" cs)
        ;
    };
    fake_capsules = rec {
        nixpkgsCurrent = rec {
            # NOTE: This is actually very wrong:
            # nixpkgs {} is impure and selects the architecture of the current
            # system. So these packages won't be good for other hosts than
            # scarif. Currently we get away with it because gently and scarif
            # have the same architecture (x86_64).
            # TODO use flakes.
            packages = (default_nixos_source.value {});
            callPackage = packages.callPackage;
        };
        nixpkgsFuture = let
            override = old: new: trace ("🕑🕐🕛 nixpkgsFuture: Replacing " + old.name + " with " + new.name) new;
        in {
            # NOTE: Each this we use something from unstable, we should pin
            # that exact source. That way, we can someday move away from the
            # unstable version when it hits the stable channels.  If we would
            # take packages directly from one perpetually updated unstable
            # source, we will never catch up.
            packages = {
                git-annex-remote-rclone = override nixpkgsCurrent.packages.git-annex-remote-rclone (nixpkgsCurrent.callPackage ./pkgs/git-annex-remote-rclone {});
            };
        };
        nextcloud = {
            packages = {
                nextcloud = nixpkgsCurrent.packages.nextcloud26;
                apps = sources.nextcloud_26_apps.value nixpkgsCurrent.packages;
            };
        };
    };
    capsules = real_capsules // fake_capsules // { all = all_capsule; };
in {
    boltons = boltons;
    default_nixos = default_nixos; # extracted by komputiloj script
    sources = sources;
    capsules = capsules;
    apps.thee = import ./apps/thee {
        inherit boltons;
        inherit capsules;
    };
}

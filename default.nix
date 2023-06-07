# TODO this file should become the anchor point for all komputiloj operations
let boltons = import ./lib/boltons.nix;
in with boltons;
let
    sources = importDir ./sources.d;
    default_nixos = "nixos_22_11"; # defines default nixos used by all of komputiloj
    default_nixos_source = getAttr default_nixos sources;
    default_nixpkgs = default_nixos_source.value {};
    komputiloj_capsule = {
        users = importDir ./users.d;
        packages = let
            callPackage = pkg: default_nixpkgs.callPackage pkg { inherit boltons; };
        in {
            wachtwoord = callPackage ./pkgs/wachtwoord;
            git-annex-remote-rclone = callPackage ./pkgs/git-annex-remote-rclone;
            radicale-commit-hook = callPackage ./pkgs/radicale-commit-hook;
        };
        overlays = mapAttrs (name: value: value { inherit boltons; })
                   (importDir ./overlays.d);
    };
    capsules = {
        komputiloj = komputiloj_capsule;
        hello-infra = sources.hello-infra.value {
            inherit boltons;
            nixpkgs = default_nixpkgs;
        };
        gorinchemindialoog = sources.gorinchemindialoog.value;

        all = {
        # special capsule which aggregates stuff from all other capsules
            users =
                # TODO merge with check for duplicates
                capsules.gorinchemindialoog.users
                //
                capsules.hello-infra.users
                //
                capsules.komputiloj.users
            ;
        };
    };
in {
    boltons = boltons;
    default_nixos = default_nixos; # extracted by komputiloj script
    sources = sources;
    capsules = capsules;
    apps.thee = import ./apps/thee {
        inherit boltons;
        users = capsules.all.users;
        nixpkgs = default_nixpkgs;
    };
}

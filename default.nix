# TODO this file should become the anchor point for all komputiloj operations
with builtins;
let
    utilecoj = import ./utilecoj.nix;
    sources = import ./sources.nix; # TODO move to ./sources.d/default.nix
    default_nixos = "nixos_22_11"; # defines default nixos used by all of komputiloj
    default_nixos_source = getAttr default_nixos sources;
    default_nixpkgs = default_nixos_source.value {};
    capsules = {
        komputiloj = sources.komputiloj.value {
            inherit utilecoj;
            nixpkgs = default_nixpkgs;
        };
        hello-infra = sources.hello-infra.value {
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
    default_nixos = default_nixos; # extracted by komputiloj script
    sources = sources;
    capsules = capsules;
    users = trace "users is DEPRECATED. Use capsules.all.users" capsules.all.users; # TODO remove
    apps.thee = import ./apps/thee {
        users = capsules.all.users;
        pkgs = default_nixpkgs;
    };
}

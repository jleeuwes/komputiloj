# TODO this file should become the anchor point for all komputiloj operations
with builtins;
let
    utilecoj = import ./utilecoj.nix;
    # TODO merge with check for duplicates
    users = sources.gorinchemindialoog.value.users
            //
            sources.hello-infra.value.users
            //
            (sources.komputiloj.value {
                pkgs = default_nixpkgs;
                inherit utilecoj;
            }).users
            ;
    sources = import ./sources.nix; # TODO move to ./sources.d/default.nix
    default_nixos = "nixos_22_11"; # defines default nixos used by all of komputiloj
    default_nixos_source = getAttr default_nixos sources;
    default_nixpkgs = default_nixos_source.value {};
in {
    users = users;
    sources = sources;
    default_nixos = default_nixos; # extracted by komputiloj script
    apps.thee = import ./apps/thee {
        users = users;
        pkgs = default_nixpkgs;
    };
}

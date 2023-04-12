# TODO this file should become the anchor point for all komputiloj operations
with builtins;
let
    users = sources.gorinchemindialoog.value.users // import ./users.d;
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

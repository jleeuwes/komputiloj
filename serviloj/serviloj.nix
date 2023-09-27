# nixops compatibility layer
with builtins;
let
    topLevel = import ../.;
    args = { inherit (topLevel) boltons; } // topLevel.capsules;
    modular = (import ./serviloj-modular.nix) args;
in {
    network.description = "Our humble all-encompassing serviloj deployment";
    network.nixpkgs = topLevel.capsules.nixpkgsCurrent.packages;
    gently2 = { config, lib, pkgs, ...}:
        (modular.gently2.nixosStuff {
            inherit config lib pkgs;
        }) // modular.gently2.nixopsStuff;
}


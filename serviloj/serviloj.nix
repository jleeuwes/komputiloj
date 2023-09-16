# nixops compatibility layer
with builtins;
let
    topLevel = import ../.;
    args = { inherit (topLevel) boltons; } // topLevel.capsules;
    modular = (import ./serviloj-modular.nix) args;
in {
    network = modular.network;
    gently2 = { config, lib, pkgs, ...}:
        (modular.gently2.nixosStuff {
            inherit config lib pkgs;
        }) // modular.gently2.nixopsStuff;
}


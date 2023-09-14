with builtins;
let
    topLevel = import ../.;
    args = { inherit (topLevel) boltons; } // topLevel.capsules;
in (import ./serviloj-modular.nix) args


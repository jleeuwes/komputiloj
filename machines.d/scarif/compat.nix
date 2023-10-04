# compatibility with nixos-rebuild
let
    topLevel = import ../..;
    capsules = topLevel.capsules;
    nixpkgsFuture = capsules.nixpkgsFuture;
    komputiloj = capsules.komputiloj;
in ((import ./.) capsules).mainModule

# compatibility with nixos-rebuild
# { config, pkgs, lib, ... }:
let
    topLevel = import ../..;
    capsules = topLevel.capsules;
    nixpkgsFuture = capsules.nixpkgsFuture;
    komputiloj = capsules.komputiloj;
in (import ./.) capsules

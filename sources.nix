# This is not a Nix concept, but a roll-your-own pinning system that replaces channels.
# Sources are used as a kind of channels in our configuration.nix.
# One entry, which must be named nixpkgs, is special:
# nixos-rebuild is hardcoded to load <nixpkgs/nixos>, which in turn loads our configuration.nix,
# so by the time configuration.nix is evaluated, the 'main' nixpkgs is already chosen.
# We work around this with some scripting that bootstraps a minimal NIX_PATH from the nixpkgs entry in this file.
#
# Some resources that might come in handy and/or inspired this stuff:
# - https://github.com/NixOS/nixpkgs/issues/62832
# - https://github.com/NixOS/nixpkgs/issues/35411#issuecomment-368172579
# - https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH
# - https://nix.dev/reference/pinning-nixpkgs
# 
# To update, run `update-sources`,
# then rebuild nixos.
#
# TODO: we might need to do some trickery to make sure the actively used sources are not gc'ed:
# https://discourse.nixos.org/t/pinned-nixpkgs-keeps-getting-garbage-collected/12912/6
with builtins;
with (import ./util.nix);
let
	source_dirs = dirnames_in ./sources.d;
	source = source_dir: import (./sources.d + "/${source_dir}");
in
listToAttrs (map (source_dir: {name = source_dir; value = source source_dir;}) source_dirs)

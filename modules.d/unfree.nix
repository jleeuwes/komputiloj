{ ... }:
# Support modular (mergeable) list of unfree packages.
# https://github.com/NixOS/nixpkgs/issues/55674
# https://github.com/NixOS/nixpkgs/pull/44518
{ config, lib, ... }:
with lib;
{
    options = {
        nixpkgsAllowUnfreePackages = mkOption {
            type = types.listOf types.str;
            description = ''
                The list of allowed unfree package names.
            '';
        };
    };

    config = {
        nixpkgs.config.allowUnfreePredicate = pkg:
            # Selectively allow some unfree packages
            # - https://nixos.org/nixpkgs/manual/#sec-allow-unfree
            builtins.elem (getName pkg) config.nixpkgsAllowUnfreePackages;
    };
}

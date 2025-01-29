# Support building for raspberry.
{ boltons, ... }:
# https://discourse.nixos.org/t/how-do-i-get-my-aarch64-linux-machine-to-build-x86-64-linux-extra-platforms-doesnt-seem-to-work/38106
# https://discourse.nixos.org/t/error-building-a-raspberry-pi-image-using-nix-on-a-x86-64-system/37968/3
{ config, ... }: {
    config = {
        boot.binfmt.emulatedSystems = ["aarch64-linux"];
        nix.settings = {
            extra-platforms = config.boot.binfmt.emulatedSystems;
            extra-substituters = [
                "https://nix-community.cachix.org"
            ];
            extra-trusted-public-keys = [
                "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            ];
        };

    };
}

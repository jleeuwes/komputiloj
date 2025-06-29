# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.11/nixos-24.11.718912.1f426f65ac4e/nixexprs.tar.xz";
        sha256 = "1shsniwaqzf2d3a8rx4bmr5jgigii32r5v5sbr9brkmp6sfnwmgm";
    };
    value = import (nix_path + "/flake.nix");
}

# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.05/nixos-24.05.3914.c3d4ac725177/nixexprs.tar.xz";
        sha256 = "1bs4sl01pbxp47sr3hny9mipfibazw1ch2b9cd6vygi501ickx9w";
    };
    value = import nix_path;
}

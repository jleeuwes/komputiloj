# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.2217.d02d818f22c7/nixexprs.tar.xz";
        sha256 = "047zq3b8w0zqpx6dq064dsbbqly8sfyjxln6dlx0mzai8w1iaa3j";
    };
    value = import nix_path;
}

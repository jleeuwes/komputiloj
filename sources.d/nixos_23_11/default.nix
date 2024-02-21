# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.4516.e0da498ad77a/nixexprs.tar.xz";
        sha256 = "0h5bfkfhh7906qh65b39dliylqfcsxlk0bl309b0ij206sk7yhzd";
    };
    value = import nix_path;
}

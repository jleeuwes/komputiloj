# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.7093.44072e24566c/nixexprs.tar.xz";
        sha256 = "0slhmi01wzni4bzlpyfqiwmn4a86hxbbn245z1lfcdcpvnk184vp";
    };
    value = import nix_path;
}

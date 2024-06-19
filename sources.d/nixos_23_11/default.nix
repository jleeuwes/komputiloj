# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.7621.842253bf992c/nixexprs.tar.xz";
        sha256 = "0jbnvgl3ffrgfqkjsqah87vd457fngzdygfc522ibipc2nh36jvn";
    };
    value = import nix_path;
}

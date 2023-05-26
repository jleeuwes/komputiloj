# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/unstable/nixos-23.05pre487203.7084250df3d/nixexprs.tar.xz";
        sha256 = "055jlwqipad2hl0y8p4g294hakkvpfyapbh7msis1s5fn51krm5f";
    };
    value = import nix_path;
}

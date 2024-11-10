# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.05/nixos-24.05.6463.83fb6c028368/nixexprs.tar.xz";
        sha256 = "1h73z8c0fp8sh9wvh80gyv99hncnzzdrah65lyb5f47z3w2hqiqc";
    };
    value = import nix_path;
}

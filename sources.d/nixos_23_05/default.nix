# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.05/nixos-23.05.4906.0c5678df521e/nixexprs.tar.xz";
        sha256 = "1byhgiyxpfbv353qn6k29zy3hhxfqiip736778bvh8q18bhq0jn9";
    };
    value = import nix_path;
}

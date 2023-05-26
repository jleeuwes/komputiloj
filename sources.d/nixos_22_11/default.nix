# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/22.11/nixos-22.11.4366.a17f99dfcb9/nixexprs.tar.xz";
        sha256 = "1zf9qc5s2mdxyksmzjcffkichvska63r6943f3a2q7ypwdn32z3m";
    };
    value = import nix_path;
}

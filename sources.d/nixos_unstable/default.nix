# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/unstable/nixos-25.11pre822156.30e2e2857ba4/nixexprs.tar.xz";
        sha256 = "1y13nd5pwsnyyvh0nl0xp037f246n8ynrhgs5q8criibk985f5p9";
    };
    value = import (nix_path + "/flake.nix");
}

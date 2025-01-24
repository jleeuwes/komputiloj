# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.11/nixos-24.11.713515.47addd76727f/nixexprs.tar.xz";
        sha256 = "1y3hb1yhsrvqsybmksw91jsigyqs1m9z2k51986f8mcc6svh58dn";
    };
    value = import (nix_path + "/flake.nix");
}

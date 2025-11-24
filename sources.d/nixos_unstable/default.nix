# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/unstable/nixos-25.11pre900642.050e09e09111/nixexprs.tar.xz";
        sha256 = "0q1ng485ch61783a3rpj4lf6izhvmw1d0djakd1hlq948p1nfd4z";
    };
    value = import (nix_path + "/flake.nix");
    value_nonflake = import nix_path;
}

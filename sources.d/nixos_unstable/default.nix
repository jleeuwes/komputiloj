# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/unstable/nixos-25.11pre861038.c23193b943c6/nixexprs.tar.xz";
        sha256 = "0iqqv8c9cbf056fvpl8cbb61qkimnzz12lj0q964zd833vl9jv5f";
    };
    value = import (nix_path + "/flake.nix");
    value_nonflake = import nix_path;
}

# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/25.05/nixos-25.05.809913.7ff837017c3b/nixexprs.tar.xz";
        sha256 = "0j9nqs9rn7vmwl9nhwc2vbpssa59jhqail85w9sf5pgcx5hy2pws";
    };
    value = import (nix_path + "/flake.nix");
    value_nonflake = import nix_path;
}

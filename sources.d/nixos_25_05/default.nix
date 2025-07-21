# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/25.05/nixos-25.05.806668.f01fe91b0108/nixexprs.tar.xz";
        sha256 = "02m7gxhfggnhyw338bba8ccza8kqic249plchv90r5jq319m2k3j";
    };
    value = import (nix_path + "/flake.nix");
}

# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.11/nixos-24.11.718975.f25c1bd2a6b3/nixexprs.tar.xz";
        sha256 = "0zrcd76bd2vrdjgrbgxndv6q6h1grh31cj11psbx907pbi26kz4w";
    };
    value = import (nix_path + "/flake.nix");
}

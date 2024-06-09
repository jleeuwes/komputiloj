# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.7503.a2e1d0414259/nixexprs.tar.xz";
        sha256 = "0f16yl63niibi57l64ijsxygvbgx394piymirvn2agabyzsy38s1";
    };
    value = import nix_path;
}

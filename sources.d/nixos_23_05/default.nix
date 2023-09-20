# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.05/nixos-23.05.3580.5d017a8822e0/nixexprs.tar.xz";
        sha256 = "1sy4gk7cvj1zhcnp31yz2hx1kydyn7by9jd7dn8qpnckaxjv1rlr";
    };
    value = import nix_path;
}

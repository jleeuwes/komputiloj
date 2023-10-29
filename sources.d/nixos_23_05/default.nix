# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.05/nixos-23.05.4527.60b9db998f71/nixexprs.tar.xz";
        sha256 = "1y95zj7kxnvi47jf6gmjlcyv29p7917mdv6847xsly9x7m2lkas1";
    };
    value = import nix_path;
}

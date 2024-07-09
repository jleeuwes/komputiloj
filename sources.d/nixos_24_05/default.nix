# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.05/nixos-24.05.2580.194846768975/nixexprs.tar.xz";
        sha256 = "1qzpw7s6sms6z1i8nch28ncl9lg8xk901afw5601iq5jhcaprs5p";
    };
    value = import nix_path;
}

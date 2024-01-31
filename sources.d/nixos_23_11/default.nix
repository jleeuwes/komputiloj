# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.3684.f4a8d6d5324c/nixexprs.tar.xz";
        sha256 = "10bkzz6isjf18wcma02kqkjnxz23xl6n0nvdnawxi6sxw6wdighq";
    };
    value = import nix_path;
}

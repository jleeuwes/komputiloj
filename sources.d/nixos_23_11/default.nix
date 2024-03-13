# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.5286.ddcd7598b218/nixexprs.tar.xz";
        sha256 = "18wzm7iixkn4gs245vmhs4wklbrqv04836nzrix78hmgiq4ad8pk";
    };
    value = import nix_path;
}

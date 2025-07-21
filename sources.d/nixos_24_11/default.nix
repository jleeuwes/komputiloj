# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.11/nixos-24.11.719113.50ab793786d9/nixexprs.tar.xz";
        sha256 = "189lwv8l10bm9fnksfkzy05pm86gmziy9z3bdn13zc8d636ylchj";
    };
    value = import (nix_path + "/flake.nix");
}

# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.11/nixos-24.11.717703.5b35d248e920/nixexprs.tar.xz";
        sha256 = "0jwyhk0js2vnxgiaalsbab1k82wl2vrfwzn08j8fs590gyivanjz";
    };
    value = import (nix_path + "/flake.nix");
}

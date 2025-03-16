# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.11/nixos-24.11.715614.cdd2ef009676/nixexprs.tar.xz";
        sha256 = "1ila630hqmk7gqiib1vhyr4rdx9ncq7ymjl7wcxcnmcsmswha2hm";
    };
    value = import (nix_path + "/flake.nix");
}

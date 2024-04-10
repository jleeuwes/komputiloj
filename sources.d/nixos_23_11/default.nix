# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/23.11/nixos-23.11.5987.d272ca50d1f7/nixexprs.tar.xz";
        sha256 = "1ccxi3sdlrknnpvjq87whvyz3pqgmvqwsihbmqhw38i0v20w9y78";
    };
    value = import nix_path;
}

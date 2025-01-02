# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/636b82f4175e3f6b1e80d2189bb0469e2ae01a55/nixos-mailserver-636b82f4175e3f6b1e80d2189bb0469e2ae01a55.tar.gz";
        sha256 = "08zdidja5kdqgskynxsmcd8skh1b7cfl9ijjy9pak4b5h3aw2iqv";
    };
    value = import nix_path;
}

# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/bc667fb6afc45f6cc2d118ab77658faf2227cffd/nixos-mailserver-bc667fb6afc45f6cc2d118ab77658faf2227cffd.tar.gz";
        sha256 = "1h1r4x2ffqwyk0ql6kjvcpg1bdiimyzhrsvn49702fsgzpx57fhd";
    };
    value = import nix_path;
}

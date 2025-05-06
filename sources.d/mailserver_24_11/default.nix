# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/14857a0309d7bbdf7c51bbfa309d0d13448ae77e/nixos-mailserver-14857a0309d7bbdf7c51bbfa309d0d13448ae77e.tar.gz";
        sha256 = "0lgrqdgb4z45ng875pa47m2vm7p3hhxn4n80x9z4qwvcdrrxrgch";
    };
    value = import nix_path;
}

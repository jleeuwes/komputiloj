# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/e47f3719f1db3e0961a4358d4cb234a0acaa7baf/nixos-mailserver-e47f3719f1db3e0961a4358d4cb234a0acaa7baf.tar.gz";
        sha256 = "122vm4n3gkvlkqmlskiq749bhwfd0r71v6vcmg1bbyg4998brvx8";
    };
    value = import nix_path;
}

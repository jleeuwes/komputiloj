# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/24128c3052090311688b09a400aa408ba61c6ee5/nixos-mailserver-24128c3052090311688b09a400aa408ba61c6ee5.tar.gz";
        sha256 = "1ngil2shzkf61qxiqw11awyl81cr7ks2kv3r3k243zz7v2xakm5c";
    };
    value = import nix_path;
}

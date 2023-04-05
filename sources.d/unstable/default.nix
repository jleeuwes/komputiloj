# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/unstable/nixos-23.05pre470275.53dad94e874/nixexprs.tar.xz";
        sha256 = "05j7m9yd1ynl90ammwj8k38ix3dn4wwmh99bj6srg7riw9hxal3f";
    };
    value = import nix_path;
}

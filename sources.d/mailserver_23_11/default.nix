# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/4bfb8eb058f098302c97b909df2d019926e11220/nixos-mailserver-4bfb8eb058f098302c97b909df2d019926e11220.tar.gz";
        sha256 = "1czvxn0qq2s3dxphpb28f3845a9jr05k8p7znmv42mwwlqwkh1ax";
    };
    value = import nix_path;
}

# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://github.com/edolstra/flake-compat/archive/f387cd2afec9419c8ee37694406ca490c3f34ee5.tar.gz";
        sha256 = "0bi4cpqmwpqkv2ikml4ryh14j5l9bl1f16wfixa97h6yvk7ik9aw";
    };
    value = import nix_path;
}

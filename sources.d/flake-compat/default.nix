# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://github.com/edolstra/flake-compat/archive/65f23138d8d09a92e30f1e5c87611b23ef451bf3.tar.gz";
        sha256 = "1c5f7vfn205bj4bmkgzgyw9504xh5j7gcwi8jf7yh581bwzlwl71";
    };
    value = import nix_path;
}

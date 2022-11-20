with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://github.com/NixOS/nixops/archive/45256745cef246dabe1ae8a7d109988f190cd7ef.tar.gz";
        sha256 = "0ni1v8ppg5cf35gq7nzd50kajxzp5zkbzhf022in0fgbjcprlzr2";
    };
    value = import nix_path;
}

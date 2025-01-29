# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://github.com/nix-community/raspberry-pi-nix/archive/628e512d60fa99f8f49e73e39b7cedf9b968c282.tar.gz";
        sha256 = "11hp809ifviwjjfrz401m9j31kz7hrj553afwwfw3ywa0kcvxkxp";
    };
    value = import nix_path;
}

# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://github.com/jleeuwes/raspberry-pi-nix/archive/824ce252585a8fbf8b50023da67c8405d5f4ff85.tar.gz";
        sha256 = "0r3hlcxikfjd4pbbalznsd0jr8ygj5idvysh6206vyq0pyjzhq6j";
    };
    value = import nix_path;
}
